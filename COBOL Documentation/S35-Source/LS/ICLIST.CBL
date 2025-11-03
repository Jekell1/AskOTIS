       IDENTIFICATION DIVISION.
       PROGRAM-ID.   ICLIST.
       DATE-WRITTEN. 02-17-84
      ******************************************************************
      *
      *                      (LS)
      *  DESCRIPTION:  INSURANCE COMPANY FILE LIST
      *
      *-----------------------------------------------------------------
      *  REV:
      *  MJD 062295 ADDED INSURANCES TO OUTPUT.
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 112196 MADE IC-COMMRATE(7) PP2
      *  KEC 042098 CONDENSE THE DETAIL-LINE TO HAVE ALL INFORMATION
      *              PRINT ON TWO LINES.  PER CINDY!!!
      *  KEC 052798 REVISED EDITOR (AGAIN) FOR A 'LIBGB/EDITOR' PROBLEM.
      *  MJD 991001 ADDED LOGIC TO COMPUTE COMMISSION AS EITHER % OR
      *             FIXED AMOUNT  (TFC PR#41)
      *  MJD 991019 FIXED BUG. TRUNCATING SIGNIFICANT DIGIT COMMRATE
      *             FIXED AMOUNTS > 99.99.
      * KEC 2018-0925 CONVERTED ICFILE FOR LOCAL-TO-GLOBAL
      * BAH 20190930 REMOVED ACCESS-CALL ICFILE
      *
      * JKC 2024-0307 REVISED TO REMOVE EDITOR COPY MEMBER AND HARDCODE.
      *
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
           "@(#)LS/ICLIST.CBL S35 03/07/24-14:50:00 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       01  PR-IFN                          PIC X(02)    VALUE "PR".

       01  FIRST-TIME                      PIC X(01)    VALUE SPACES.

       01  LEGEND                          PIC X(60)    VALUE SPACES.
       01  LEGEND-LIST                     PIC X(07)    VALUE "LEGEND.".

       01  PROGRAM-EXIT-STATUS             PIC X(01)    VALUE "P".
           88  PROGRAM-EXIT-AFTER-RUN                   VALUE "A".
           88  PROGRAM-EXIT-PRIOR-RUN                   VALUE "P".

       01  PROGRESS-LINE-TEXT.
           03  FILLER                      PIC X(30)    VALUE
               "INSURANCE COMPANY LISTING IN P".
           03  FILLER                      PIC X(30)    VALUE
               "ROGRESS.......................".

       01  RECORDS-FOUND                   PIC X(01)    VALUE SPACES.
           88  NO-RECORDS-FOUND                         VALUE "N".

       01  SUBSCRIPTS.
           03  SUB                         PIC 9(03).

       01  ERRCD                        PIC X(01).

      *=================================================================
      *
      *    S C R E E N - R A N G E S
      *
      *=================================================================
       01  SCREEN-RANGES.
           05  BEG-BRANCH               PIC 9(04).
           05  END-BRANCH               PIC 9(04).
           05  BEG-CODE                 PIC 9(02).
           05  END-CODE                 PIC 9(02).
           05  PRINT-TYPE               PIC X(01).
               88  PRINT-TYPE-FULL                     VALUE "F".
               88  PRINT-TYPE-COMMRATE                 VALUE "C".
           05  GROUP-FLAG               PIC X(01).
           05  ENT-GROUP                PIC X(04).

      *-----------------------------------------------------------------
       01  SCREEN-RANGES-LIST.
           03  FILLER                   PIC X(51)      VALUE
               "BEG-BRANCH END-BRANCH BEG-CODE END-CODE PRINT-TYPE.".

      *=================================================================
      *
      *    L I N E - W O R K E R S
      *
      *=================================================================
       01  LN-WORKERS.
           03  LN-FEED                       PIC 9(02)     VALUE ZEROES.
           03  LN-COUNT                      PIC 9(03)     VALUE 99.
           03  LN-WK                         PIC 9(03)     VALUE ZEROES.

       01  PAGE-NUMBER                       PIC 9(04)     VALUE ZEROES.

      *=================================================================
      *
      *    H E A D E R - B U F F E R S
      *
      *=================================================================
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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(24)    VALUE SPACES.
           03  FILLER           PIC X(43)    VALUE
               " - - - INSURANCE COMPANY FILE LISTING - - -".
           03  FILLER           PIC X(45)    VALUE SPACES.


      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH :".
           03  H-BRNO           PIC 9(04).
           03  FILLER           PIC X(03)    VALUE SPACES.
           03  H-BRDESC         PIC X(40).
           03  FILLER           PIC X(75)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(04)     VALUE "CODE".
           03  FILLER           PIC X(01)     VALUE SPACES.
           03  FILLER           PIC X(20)     VALUE "NAME".
           03  FILLER           PIC X(01)     VALUE SPACES.
           03  FILLER           PIC X(50)     VALUE "ADDRESS".
           03  FILLER           PIC X(01)     VALUE SPACES.
           03  FILLER           PIC X(15)     VALUE "CITY".
           03  FILLER           PIC X(01)     VALUE SPACES.
           03  FILLER           PIC X(02)     VALUE "ST".
           03  FILLER           PIC X(01)     VALUE SPACES.
           03  FILLER           PIC X(10)     VALUE "ZIPCODE".
           03  FILLER           PIC X(01)     VALUE SPACES.
           03  FILLER           PIC X(14)     VALUE "PHONE NUMBER".
           03  FILLER           PIC X(12)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD5-FULL.
           03  FILLER      PIC X(22)    VALUE "--- DECLINING LIFE ---".
           03  FILLER      PIC X(22)    VALUE " ---- WHOLE LIFE -----".
           03  FILLER      PIC X(11)    VALUE "   ACCIDENT".
           03  FILLER      PIC X(22)    VALUE "   PERSONAL PROPERTY  ".
           03  FILLER      PIC X(55)    VALUE
              "  ----------------- OTHER INSURANCES ------------------".

      *-----------------------------------------------------------------
       01  HEAD5-COMMRATE.
           03  FILLER      PIC X(22)    VALUE "INS - DECLINING LIFE -".
           03  FILLER      PIC X(22)    VALUE " ---- WHOLE LIFE -----".
           03  FILLER      PIC X(11)    VALUE "   ACCIDENT".
           03  FILLER      PIC X(22)    VALUE "   PERSONAL PROPERTY  ".
           03  FILLER      PIC X(55)    VALUE
              "  ----------------- OTHER INSURANCES ------------------".

      *-----------------------------------------------------------------
       01  HEAD6-FULL.
           03  FILLER           PIC X(11)     VALUE "   (SINGLE)".
           03  FILLER           PIC X(11)     VALUE "    (JOINT)".
           03  FILLER           PIC X(11)     VALUE "   (SINGLE)".
           03  FILLER           PIC X(11)     VALUE "    (JOINT)".
           03  FILLER           PIC X(11)     VALUE "   & HEALTH".
           03  FILLER           PIC X(11)     VALUE "   (SINGLE)".
           03  FILLER           PIC X(11)     VALUE "     (DUAL)".
           03  FILLER           PIC X(11)     VALUE "        (1)".
           03  FILLER           PIC X(11)     VALUE "        (2)".
           03  FILLER           PIC X(11)     VALUE "        (3)".
           03  FILLER           PIC X(11)     VALUE "        (4)".
           03  FILLER           PIC X(11)     VALUE "        (5)".

      *-----------------------------------------------------------------
       01  HEAD6-COMMRATE.
           03  FILLER           PIC X(11)     VALUE "CD (SINGLE)".
           03  FILLER           PIC X(11)     VALUE "    (JOINT)".
           03  FILLER           PIC X(11)     VALUE "   (SINGLE)".
           03  FILLER           PIC X(11)     VALUE "    (JOINT)".
           03  FILLER           PIC X(11)     VALUE "   & HEALTH".
           03  FILLER           PIC X(11)     VALUE "   (SINGLE)".
           03  FILLER           PIC X(11)     VALUE "     (DUAL)".
           03  FILLER           PIC X(11)     VALUE "        (1)".
           03  FILLER           PIC X(11)     VALUE "        (2)".
           03  FILLER           PIC X(11)     VALUE "        (3)".
           03  FILLER           PIC X(11)     VALUE "        (4)".
           03  FILLER           PIC X(11)     VALUE "        (5)".

      *=================================================================
      *
      *    D E T A I L - L I N E - B U F F E R
      *
      *=================================================================
       01  DETAIL-LINE                      PIC X(132)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE1                REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(01).
           03  D-CODE                       PIC 9(02).
           03  FILLER                       PIC X(02).
           03  D-NAME                       PIC X(20).
           03  FILLER                       PIC X(01).
           03  D-ADDR                       PIC X(50).
           03  FILLER                       PIC X(01).
           03  D-CITY                       PIC X(15).
           03  FILLER                       PIC X(01).
           03  D-STATE                      PIC X(02).
           03  FILLER                       PIC X(01).
           03  D-ZIP                        PIC X(10).
           03  FILLER                       PIC X(01).
           03  D-PHONE                      PIC X(14).
           03  FILLER                       PIC X(10).

      *-----------------------------------------------------------------
       01  D-LINE2                REDEFINES DETAIL-LINE.
           03  D-COMMRATE-TABLE             OCCURS 12 TIMES.
               05  D-INS-CODE               PIC X(02).
               05  FILLER                   PIC X(01).
               05  D-COMMRATE               PIC ZZZ9.99.
               05  D-COMMRATE-2      REDEFINES D-COMMRATE
                                            PIC $$$9.99.
               05  D-PERCENT                PIC X(01).

      *-----------------------------------------------------------------
       01  RANGE-LINE             REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(20).
           03  RANGE-SELECTION              PIC X(16).
           03  RANGE-BRANCH-NUM   REDEFINES RANGE-SELECTION.
               05  RANGE-BR-NO              PIC Z(04).
               05  FILLER                   PIC X(12).

      *=================================================================
      *
      *    E D I T O R - T E L E P H O N E
      *
      *------------[ INPUT ]----( AAA1112222 )-----[ OUTPUT ]-----------
      *  EDITOR-INPUT-PHONE            |  EDITOR-OUTPUT-PHONE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  EDITOR-PHONE
      *  EDITOR-PHONE-SHORT
      *
      *------------[ INPUT ]--( (AAA)-111-2222 )---[ OUTPUT ]-----------
      *  EDITOR-INPUT-PHONE-EDIT       |  EDITOR-OUTPUT-PHONE-EDIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  EDITOR-PHONE-EDIT
      *  EDITOR-PHONE-SHORT-EDIT
      *
      *=================================================================

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FORMAT: AAA1112222
      *    NAME  : EDITOR-PHONE
      *            EDITOR-PHONE-SHORT
      *            EDITOR-INPUT-PHONE
      *            EDITOR-OUTPUT-PHONE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  EDITOR-PHONE                                       PIC 9(10).
       01  EDITOR-INPUT-PHONE          REDEFINES EDITOR-PHONE PIC 9(10).
       01  EDITOR-OUTPUT-PHONE         REDEFINES EDITOR-PHONE PIC 9(10).
       01  EDITOR-PHONE-RDF            REDEFINES EDITOR-PHONE.
           03  EDITOR-PHONE-AREA                              PIC 9(3).
           03  EDITOR-PHONE-AREA-CODE  REDEFINES EDITOR-PHONE-AREA
                                                              PIC 9(3).
           03  EDITOR-PHONE-SHORT                             PIC 9(7).
           03  EDITOR-PHONE-STANDARD   REDEFINES EDITOR-PHONE-SHORT.
               05  EDITOR-PHONE-3                             PIC 9(3).
               05  EDITOR-PHONE-4                             PIC 9(4).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FORMAT: (AAA)-111-2222
      *                  111-2222
      *    NAME  : EDITOR-PHONE-EDIT
      *            EDITOR-PHONE-SHORT-EDIT
      *            EDITOR-INPUT-PHONE-EDIT
      *            EDITOR-OUTPUT-PHONE-EDIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  EDITOR-PHONE-EDIT                                  PIC X(14).
       01  EDITOR-INPUT-PHONE-EDIT     REDEFINES EDITOR-PHONE-EDIT
                                                              PIC X(14).
       01  EDITOR-OUTPUT-PHONE-EDIT    REDEFINES EDITOR-PHONE-EDIT
                                                              PIC X(14).
       01  EDITOR-PHONE-EDIT-RDF       REDEFINES EDITOR-PHONE-EDIT.
           03  EDITOR-PHONE-EDIT-P1                           PIC X(1).
           03  EDITOR-PHONE-EDIT-AREA                         PIC 9(3).
           03  EDITOR-PHONE-EDIT-P2                           PIC X(1).
           03  EDITOR-PHONE-EDIT-D1                           PIC X(1).
           03  EDITOR-PHONE-SHORT-EDIT                        PIC X(8).
           03  EDITOR-PHONE-SHT-EDITX REDEFINES EDITOR-PHONE-SHORT-EDIT.
               05  EDITOR-PHONE-EDIT-3                        PIC 9(3).
               05  EDITOR-PHONE-EDIT-D2                       PIC X(1).
               05  EDITOR-PHONE-EDIT-4                        PIC 9(4).

       01  ICLIST-WINDOW-HANDLE PIC X(10).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/ICLIST_DEF.CPY".
       COPY "LIBLS/ICLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/ICLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-IC1-FILE.
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

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

       BEGIN-GROUP-LOCALITY.

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
              GO TO END-MAIN-MODULE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES ) OR ( BR-NO > END-BRANCH )
              GO TO END-MAIN-MODULE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO IC-PATH-OVERRIDE.

           PERFORM PROCESS-IC-FILE.

           PERFORM CLOSE-IC1-FILE.

           MOVE "Y" TO FIRST-TIME.
           GO TO NEXT-GROUP-LOCALITY.

       END-MAIN-MODULE.

           PERFORM PRINT-RANGE-LINES.

       EXIT-MAIN-MODULE.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ICLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ICLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.


           PERFORM ENTER-PARAMETERS.

           IF ( ERRCD = "X" )
              GO TO END-INITIALIZATION.


           MOVE PROGRESS-LINE-TEXT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

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
           MOVE ICLIST-QUEUE-POS TO Q-POS.
           MOVE ICLIST-COPIES-POS TO C-POS.
           MOVE ICLIST-PRU-POS TO P-POS.
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

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"                  TO GROUP-FLAG.
           MOVE WGR-SRESULT          TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
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
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY"     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BRANCHES.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N"                    TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES                 TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

       
       ENTER-BEG-CODE.

           MOVE "E  A010BEG-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F3-KEY ) GO TO ALL-CODES.
           IF (                    F7-KEY ) GO TO END-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-END-BRANCH.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-CODE.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-BEG-CODE
           ELSE
              MOVE VDUBUF TO BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-END-CODE.

           MOVE "E  A010END-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F3-KEY ) GO TO ALL-CODES.
           IF (                    F7-KEY ) GO TO END-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-BEG-CODE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-CODE.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-END-CODE
           ELSE
              MOVE VDUBUF TO END-CODE.

           IF ( BEG-CODE > END-CODE )
               MOVE " INVALID CODE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-BEG-CODE.

           GO TO ENTER-PRINT-TYPE.

      *-----------------------------------------------------------------
       ALL-CODES.
           MOVE "S  A010BEG-CODE." TO VDU.
           MOVE "00"               TO VDUBUF BEG-CODE.
           PERFORM SCREENIO.

           MOVE "S  A010END-CODE." TO VDU.
           MOVE "99"               TO VDUBUF END-CODE.
           PERFORM SCREENIO.

       
       ENTER-PRINT-TYPE.

           MOVE "E  A010PRINT-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F7-KEY ) GO TO END-INITIALIZATION.
           IF (                    UP-KEY ) GO TO ENTER-END-CODE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PRINT-TYPE.

           IF NOT ( VDUBUF = "C" OR "F" )
              GO TO ENTER-PRINT-TYPE.

           MOVE VDUBUF TO PRINT-TYPE.

       
       ENTER-ACCEPT-YN.

           MOVE SPACES           TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F7-KEY ) GO TO END-INITIALIZATION.
           IF (                    UP-KEY ) GO TO ENTER-PRINT-TYPE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

           IF ( VDUBUF         = "N"  ) GO TO ENTER-BEG-BRANCH.
           IF ( VDUBUF         = "Y"  ) GO TO EXIT-ENTER-PARAMETERS.

           GO TO ENTER-ACCEPT-YN.

       
       END-ENTER-PARAMETERS.

           MOVE "X" TO ERRCD.

       EXIT-ENTER-PARAMETERS.
           EXIT.

      ******************************************************************
       PROCESS-IC-FILE SECTION.
      ******************************************************************

           MOVE 99       TO LN-COUNT.
          
           PERFORM OPEN-IC1-FILE.

           MOVE IC-PATH-OWNBR TO IC-BRNO
                                 QIC1-WBEG-BRNO
                                 QIC1-WEND-BRNO.
           MOVE BEG-CODE      TO IC-CODE
                                 QIC1-WBEG-CODE.
           MOVE END-CODE      TO QIC1-WEND-CODE.

           PERFORM START-IC1-FILE.
       
       NEXT-IC-RECORD.

           PERFORM READ-IC1-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES ) OR ( IC-BRNO NOT = IC-PATH-OWNBR )
               GO TO EXIT-PROCESS-IC-FILE.

           IF ( IC-CODE > END-CODE )
               GO TO EXIT-PROCESS-IC-FILE.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-IC-RECORD.

       EXIT-PROCESS-IC-FILE.
           EXIT.

      ******************************************************************
      *
      *    E D I T O R - P H O N E - F O R M A T - E D I T
      *
      *-----------------------------------------------------------------
      *    DESC: MOVE TO NEW PHONE FORMAT: AAA1112222 TO (AAA) 111-2222
      *    IN  : EDITOR-INPUT-PHONE
      *    OUT : EDITOR-OUTPUT-PHONE-EDIT
      *          EDITOR-PHONE-SHORT-EDIT
      ******************************************************************
       EDITOR-PHONE-FORMAT-EDIT SECTION.

           MOVE "("               TO EDITOR-PHONE-EDIT-P1.
           MOVE EDITOR-PHONE-AREA TO EDITOR-PHONE-EDIT-AREA.
           MOVE ")"               TO EDITOR-PHONE-EDIT-P2.
           MOVE " "               TO EDITOR-PHONE-EDIT-D1.
           MOVE EDITOR-PHONE-3    TO EDITOR-PHONE-EDIT-3.
           MOVE "-"               TO EDITOR-PHONE-EDIT-D2.
           MOVE EDITOR-PHONE-4    TO EDITOR-PHONE-EDIT-4.

       EXIT-PHONE-FORMAT-EDIT.
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

           IF ( PRINT-TYPE-FULL )
              MOVE HEAD4 TO PR-REC
              MOVE 2     TO LN-FEED
              PERFORM WRITE-PR-REC.

           IF ( PRINT-TYPE-FULL )
              MOVE HEAD5-FULL     TO PR-REC
              MOVE 1              TO LN-FEED
           ELSE
              MOVE HEAD5-COMMRATE TO PR-REC
              MOVE 3              TO LN-FEED.

           PERFORM WRITE-PR-REC.

           IF ( PRINT-TYPE-FULL )
              MOVE HEAD6-FULL     TO PR-REC
           ELSE
              MOVE HEAD6-COMMRATE TO PR-REC.

           MOVE 1 TO LN-FEED
           PERFORM WRITE-PR-REC.

           MOVE ALL "-" TO PR-REC.
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

           IF ( PRINT-TYPE-FULL )
              PERFORM DETAIL-LINE-FULL
           ELSE
              PERFORM DETAIL-LINE-COMMRATE.

           GO TO EXIT-DETAIL-LINE-PROCESS.

       
       DETAIL-LINE-FULL.

           MOVE IC-CODE TO D-CODE.
           MOVE IC-NAME TO D-NAME.

           STRING IC-ADDR1," ",IC-ADDR2
                  DELIMITED BY "  " INTO D-ADDR.

           MOVE IC-CITY                  TO D-CITY.
           MOVE IC-STATE                 TO D-STATE.
           MOVE IC-ZIP                   TO D-ZIP.
           MOVE IC-PHONE                 TO EDITOR-INPUT-PHONE.
           PERFORM EDITOR-PHONE-FORMAT-EDIT.
           MOVE EDITOR-OUTPUT-PHONE-EDIT TO D-PHONE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
                   MOVE IC-COMMRATE(SUB) TO D-COMMRATE(SUB)
                   IF IC-COMMRATE(SUB) >= 0
                      MOVE IC-COMMRATE(SUB) TO D-COMMRATE(SUB)
                      MOVE "%" TO D-PERCENT(SUB)
                   ELSE
                      MOVE IC-COMMRATE(SUB) TO D-COMMRATE-2(SUB)
                   END-IF
           END-PERFORM.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
       DETAIL-LINE-COMMRATE.

           MOVE IC-CODE TO D-INS-CODE(1).

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
                   MOVE IC-COMMRATE(SUB) TO D-COMMRATE(SUB)
                   IF IC-COMMRATE(SUB) >= 0
                      MOVE IC-COMMRATE(SUB) TO D-COMMRATE(SUB)
                      MOVE "%" TO D-PERCENT(SUB)
                   ELSE
                      MOVE IC-COMMRATE(SUB) TO D-COMMRATE-2(SUB)
                   END-IF
           END-PERFORM.

           MOVE 1 TO LN-FEED.
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
              MOVE "GROUP ID           :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              MOVE 1                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-BR-NO
              MOVE 1                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH   :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-BR-NO
              MOVE 1                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CODE     :" TO RANGE-LINE.
           MOVE BEG-CODE               TO RANGE-SELECTION.
           MOVE 2                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING CODE        :" TO RANGE-LINE.
           MOVE END-CODE               TO RANGE-SELECTION.
           MOVE 1                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT TYPE         :" TO RANGE-LINE.
           MOVE PRINT-TYPE             TO RANGE-SELECTION.
           MOVE 2                      TO LN-FEED.
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
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO ICLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ICLIST-SYSDATE.
           DISPLAY ICLIST-SCREEN UPON ICLIST-WINDOW-HANDLE.
           MOVE ICLIST-FORM-FIELDS TO WR-BUF.
           MOVE ICLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE ICLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE ICLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/ICLIST_EVA.CPY".
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
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       
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
           DESTROY ICLIST-SCREEN.
           DESTROY ICLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
