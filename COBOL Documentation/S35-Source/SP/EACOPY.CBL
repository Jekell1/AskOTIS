       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EACOPY.
       DATE-WRITTEN.    061484.
      *****************************************************************
      *                 (SP)
      *  DESCRIPTION:   EXTRACTION COPY UTILITY 
      * REV.
      *  KEC 032096 ADDED WK-GROUP-ID.
      *  MJD 070897 ADDED TARGET CLASS RANGE. DRIVES OFF LCFILE
      *             FOR CLASS RANGE.
      *  MJD 070997 FIXED GROUP ID SCAN. VERIFY SOURCE CLASS. VERIFY
      *             MATCHING CD-EA-CQ-FG FOR BOTH SOURCE AND TARGET
      *             GROUP ID. ADDED PASSWORD TO TARGET
      *             BRANCH NOT = USER BRANCH AND OVERWRITE.
      *  MJD 112597 EXPANDED EA-REC TO 222 BYTES
      *  BLV 121500 ADDED LOGIC TO INITIALIZE CDSCAN-EA-GQ-FG, AND
      *             VERIFY TARGET GROUP CORRESPONDS TO SOURCE GROUP
      *             FOR GLOBAL QUEUING OPTION, TFC PR# 60
      *  BLV 010102 STOP IF BRANCH GLOBAL QUEUING FLAGS NOT = FOR
      *             SOURCE & TARGET BRANCHES, TFC PR# 60
      *  BLF 051007 PROGRAM WAS ALLOWING USER TO UP ARROW & CHANGE
      *             SOURCE OR TARGET PATHS, BUT THESE WERE ONLY USED
      *             FOR EAFILE.  BRANCH, GROUP (CDFILE) & CLASS WERE
      *             GETTING VERIFIED IN RELATIVE USER FILE SET.  FIXED TO
      *             GET CORRECT PATHNAME FOR BR, CD, LC. WORLD, PER JR
      *  BLM 101105 CHANGE TARGET BRANCH ENTRY TO TARGET BRANCH RANGE OR
      *             A GROUP.  PASSWORD FOR THIS OPTION IS SP/EACOPY/2.
      *             PATH FOR GRFILE IS ALWAYS TARGET DATA PATH, GRFILE
      *             NOT USED IN SOURCE PARAMETERS.  BRFILE PATH VARIES
      *             BETWEEN SOURCE & TARGET.  ADDED DISPLAYING COUNT
      *             OF RECORDS WRITTEN & REWRITTEN. WORLD PR# 719.
      * KEC 2016.0301 FIXED KEYS & RECS FOR EA-REC (HOLD/ORIG/SAVE/WK).
      *               CHANGED CLNO/CLASS 2 TO 3 BYTES.
      * KEC 2016.0310 ADDED CD-EA-FILLER FOR CHANGE TO CD-EA-REC.
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * KEC 2018-1022 CONVERTED %%FILE FOR A32 & A34
      * BAH 181102 GLOBAL EAFILE, G2G
      * BAH 20190930 REMOVED ACCESS-CALL SOURCE & TARGET
      * BAH 20191114 REMOVED SOURCE & TARGET
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.



      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/EACOPY.CBL S35 02/23/24-11:29:13 >".

       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01EA.CPY".
       COPY "LIBLP/LP01EA_SQL.CPY".
       COPY "LIBLP/LPWSEA.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

       01  MESS-LIST       PIC X(5) VALUE "MESS.".

       01  ERRCD           PIC X    VALUE " ".

       COPY "LIBLP/LP01EA.CPY" REPLACING LEADING "EA" BY "EAOR".

       01  IO-TYPE         PIC X(3).
       01  RECORDS-UPDATED PIC X  VALUE "N".
           88 RECORDS-WRITTEN     VALUE "Y".
       01  WRITE-COUNT     PIC 9(6) VALUE 0.
       01  REWRITE-COUNT   PIC 9(6) VALUE 0.

       01  HOLD-EA-CQ-FG          PIC X.
       01  HOLD-EA-GQ-FG          PIC X.
       01  SOURCE-BR-GLOBAL-QUEUE PIC X.

      *-----------------------------------------------------------------
      * SCAN ERR FG:
      *              0 - SCAN IS SUCCESSFUL
      *              9 - SCAN WAS UNSUCCESSFUL
      *-----------------------------------------------------------------
       01  SCAN-ERR-FG        PIC 9.

       01  TARGET-EA1-KEY.
           03  TARGET-EA-BRANCH     PIC 9(4).
           03  TARGET-EA-CLASS      PIC 9(3).
           03  TARGET-EA-GROUP-ID   PIC X(6).
           03  TARGET-EA-SEQNO      PIC 99.

       01  INPUT-SOURCE-RANGES.
           03  S-BRNO        PIC 9(4).
           03  S-BEG-CLASS   PIC 99.
           03  S-END-CLASS   PIC 99.
           03  S-BEG-GROUP   PIC X(6).
           03  S-END-GROUP   PIC X(6).
           03  S-BEG-SEQNO   PIC 99.
           03  S-END-SEQNO   PIC 99.

      *-----------------------------------------------------------------
      * INPUT SOURCE RANGE FALGS:
      *   CLASS-RANGE: "Y" - SOURCE CLASS RANGE HAS BEEN SELECTED
      *                      NO INPUT IS ALLOWED TO TARGET CLASS RANGE
      *                "N" - SINGLE SOURCE CLASS IN ENTERED
      *                      TARGET CLASS ENTRY IS ALLOWED.
      *   GROUP-RANGE: "Y" - SOURCE GROUP RANGE HAS BEEN SELECTED
      *                      NO INPUT IS ALLOWED TO TARGET GROUP RANGE
      *                "N" - SINGLE SOURCE GROUP IN ENTERED
      *                      TARGET GROUP ENTRY IS ALLOWED.
      *   SEQNO-RANGE: "Y" - SOURCE SEQNO RANGE HAS BEEN SELECTED
      *                      NO INPUT IS ALLOWED TO TARGET SEQNO RANGE
      *                "N" - SINGLE SOURCE CLASS IN ENTERED
      *                      TARGET CLASS ENTRY IS ALLOWED.
      *   NOTE: TARGET CLASS RANGE IS ALLOWED ONLY WHEN ALL ABOVE
      *         FLAGS ARE "N"
      *-----------------------------------------------------------------
       01  INPUT-SOURCE-RANGE-FLAGS.
           03  CLASS-RANGE PIC X     VALUE " ".
               88 NO-CLASS-RANGE     VALUE "N".
           03  GROUP-RANGE PIC X     VALUE " ".
               88 NO-GROUP-RANGE     VALUE "N".
           03  SEQNO-RANGE PIC X     VALUE " ".
               88 NO-SEQNO-RANGE     VALUE "N".

       01  INPUT-TARGET-RANGES.
           03  BEG-TARGET-BR PIC 9(4).
           03  END-TARGET-BR PIC 9(4).
           03  D-BEG-CLASS   PIC 99.
           03  D-END-CLASS   PIC 99.
           03  D-GROUP       PIC X(6).
           03  D-SEQNO       PIC 99.
           03  OVERWRITE-FG  PIC X.
           03  GROUP-FLAG    PIC X.
           03  ENT-GROUP     PIC X(4).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       01  EACOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/EACOPY_DEF.CPY".
       COPY "LIBSP/EACOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/EACOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EACOPY"      TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS EACOPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.

           MOVE "Y"                 TO EA-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE    TO EA-ALL-BASE.
           MOVE EXT-FILPATH-MACHINE TO EA-ALL-MACHINE.
           MOVE EA-ALL-PATH         TO EA-PATH.

           PERFORM OPEN-EA1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-LC1-FILE.

       ENTER-KEYS.
           PERFORM ENTER-RANGES.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

      *****************************************************
           PERFORM COPY-RECORDS.
      *****************************************************

           GO TO ENTER-KEYS.

      ******************************************************************
      *
      *    E N T E R  R A N G E S
      *
      ******************************************************************
       ENTER-RANGES SECTION.

      *-----------------------------------------------------------------
       ENTER-BRNO.
           MOVE "ENTER SOURCE BRANCH" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO IO-TYPE.
           MOVE "ENNN040S-BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-BRNO.

           MOVE VDUNUM0 TO S-BRNO BR-NO BR-NO.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRNO.

           MOVE BR-GLOBAL-QUEUE TO SOURCE-BR-GLOBAL-QUEUE.

      *-----------------------------------------------------------------
       ENTER-BEG-CL.
           MOVE "F3-ALL, F6-SCAN, ENTER BEGINNING SOURCE CLASS" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020S-BEGCLASS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO ENTER-BRNO.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "16" GO TO ENTER-ALL-CLASS.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF SCAN-ERR-FG = 9
                 GO TO ENTER-BEG-CL
              ELSE
                 MOVE "S  N020S-BEGCLASS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-BEG-CL.

           MOVE VDUNUM0 TO S-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CL.
           MOVE "F6-SCAN, ENTER ENDING SOURCE CLASS" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020S-ENDCLASS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO ENTER-BEG-CL.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF SCAN-ERR-FG = 9
                 GO TO ENTER-END-CL
              ELSE
                 MOVE "S  N020S-ENDCLASS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-BEG-CL.

           MOVE VDUNUM0 TO S-END-CLASS.
           IF S-END-CLASS < S-BEG-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-CL.

           IF S-BEG-CLASS = S-END-CLASS
              MOVE "N" TO CLASS-RANGE
           ELSE
              MOVE "Y" TO CLASS-RANGE.

           GO TO ENTER-BEG-GROUP.
      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.
           MOVE "SNNN020S-BEGCLASS." TO VDU.
           MOVE 0 TO S-BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020S-ENDCLASS." TO VDU.
           MOVE 99 TO S-END-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "Y" TO CLASS-RANGE.

      *-----------------------------------------------------------------
       ENTER-BEG-GROUP.
           MOVE "F3-ALL, F6-SCAN, ENTER BEGINNING SOURCE GROUP ID"
                                                              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNA060S-BEGGROUP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO ENTER-END-CL.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "16" GO TO ENTER-ALL-GROUP.
           IF VDUSTATUS = "1<"
              MOVE " " TO CDSCAN-EA-CQ-FG CDSCAN-EA-GQ-FG
              PERFORM CODE-SCAN
              IF SCAN-ERR-FG = 9
                 GO TO ENTER-BEG-GROUP
              ELSE
                 MOVE "S  A060S-BEGGROUP." TO VDU
                 MOVE CDSCAN-CODE TO VDUBUF EA-GROUP-ID
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-BEG-GROUP.

           MOVE "EA"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE  S-BEG-GROUP.

           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              GO TO ENTER-BEG-GROUP.

           MOVE CD-EA-CQ-FG TO HOLD-EA-CQ-FG.
           MOVE CD-EA-GQ-FG TO HOLD-EA-GQ-FG.

      * DISPLAY GROUP DESCRIPTION.
           MOVE "S  A020S-BEGGRPDESC." TO VDU.
           MOVE CD-DESC                TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-GROUP.
           MOVE "F6-SCAN, ENTER ENDING SOURCE GROUP ID" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNA060S-ENDGROUP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO ENTER-BEG-GROUP.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1<"
              MOVE " " TO CDSCAN-EA-CQ-FG CDSCAN-EA-GQ-FG
              PERFORM CODE-SCAN
              IF SCAN-ERR-FG = 9
                 GO TO ENTER-END-GROUP
              ELSE
                 MOVE "S  A060S-ENDGROUP." TO VDU
                 MOVE CDSCAN-CODE TO VDUBUF EA-GROUP-ID
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-END-GROUP.

           MOVE "EA"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE S-END-GROUP.

           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              GO TO ENTER-END-GROUP.

           IF S-END-GROUP < S-BEG-GROUP
              MOVE "INVALID GROUP ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-GROUP.

      * DISPLAY GROUP DESCRIPTION.
           MOVE "S  A020S-ENDGRPDESC." TO VDU.
           MOVE CD-DESC                TO VDUBUF.
           PERFORM SCREENIO.

           IF S-BEG-GROUP = S-END-GROUP
              MOVE "N" TO GROUP-RANGE
           ELSE
              MOVE "Y" TO GROUP-RANGE.

           GO TO ENTER-BEG-SEQ.
      *-----------------------------------------------------------------
       ENTER-ALL-GROUP.
           MOVE "SNNA060S-BEGGROUP." TO VDU.
           MOVE "      " TO S-BEG-GROUP VDUBUF.
           PERFORM SCREENIO.
           MOVE "SNNA060S-ENDGROUP." TO VDU.
           MOVE ALL "Z" TO S-END-GROUP.
           CALL "C$TOLOWER" USING S-END-GROUP, VALUE 6.
           MOVE S-END-GROUP TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020S-BEGGRPDESC." TO VDU.
           MOVE "ALL GROUPS" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020S-ENDGRPDESC." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "Y" TO GROUP-RANGE.

      *-----------------------------------------------------------------
       ENTER-BEG-SEQ.
           MOVE "F3-ALL, ENTER BEGINNING SOURCE SEQNO" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020S-BEGSEQNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO ENTER-END-GROUP.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "16" GO TO ENTER-ALL-SEQNO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-BEG-SEQ.

           MOVE VDUNUM0 TO S-BEG-SEQNO.
      *-----------------------------------------------------------------
       ENTER-END-SEQ.
           MOVE "ENTER ENDING SOURCE SEQNO" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020S-ENDSEQNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO ENTER-BEG-SEQ.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-END-SEQ.

           MOVE VDUNUM0 TO S-END-SEQNO.

           IF S-END-SEQNO < S-BEG-SEQNO
              MOVE "INVALID SEQUENCE NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-SEQ.

           IF S-BEG-SEQNO = S-END-SEQNO
              MOVE "N" TO SEQNO-RANGE
           ELSE
              MOVE "Y" TO SEQNO-RANGE.

           GO TO ENTER-TBRNO1.
      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.
           MOVE "SNNN020S-BEGSEQNO." TO VDU.
           MOVE 0 TO S-BEG-SEQNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020S-ENDSEQNO." TO VDU.
           MOVE 99 TO S-END-SEQNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "Y" TO SEQNO-RANGE.

      *-----------------------------------------------------------------
      * TARGET KEY
      *-----------------------------------------------------------------

       ENTER-TBRNO1.
           MOVE "ENTER BEGINNING TARGET BRANCH" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF VDUSTATUS = "1U" GO TO ENTER-TBRNO1.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-TBRNO1
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-TBRNO1.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TBRNO1.

      * CHECK MEMBER CLOSES BRANCH FILE!
           PERFORM OPEN-BR-FILE.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-TARGET-BR
              MOVE 9999 TO END-TARGET-BR
              GO TO CHECK-BRANCH-GLOBAL-QUEUEING.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-TARGET-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-TARGET-BR    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-TBRNO2.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1U" GO TO ENTER-TBRNO1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-TBRNO2.

           MOVE VDUNUM0 TO END-TARGET-BR.

           IF BEG-TARGET-BR > END-TARGET-BR
              MOVE "INVALID BRANCH RANGE." TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TBRNO1.

           GO TO CHECK-BRANCH-GLOBAL-QUEUEING.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-TARGET-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-TARGET-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-GLOBAL-QUEUEING.
           IF BEG-TARGET-BR NOT = END-TARGET-BR
              GO TO SKIP-SINGLE-BRANCH-CHECKS.

           MOVE BEG-TARGET-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TBRNO1.

      * HEY, THIS FLAG MAY BE Y, N OR BLANK SO CHECK CAREFULLY:
           IF (BR-GLOBAL-QUEUE = "Y" AND
               SOURCE-BR-GLOBAL-QUEUE NOT = "Y")
           OR (SOURCE-BR-GLOBAL-QUEUE = "Y" AND
               BR-GLOBAL-QUEUE NOT = "Y")
              MOVE "GLOBAL QUEUING MISMATCH ON SELECTED BRANCHES"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TBRNO1.

           IF BEG-TARGET-BR NOT = EXT-CONAME-USER-LOC
              MOVE "TO WRITE TO ANOTHER BRANCH" TO PASSWDW-PROMPT2
              MOVE 0 TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-TBRNO1.
              GO TO ENTER-TBEG-CL.

       SKIP-SINGLE-BRANCH-CHECKS.
           MOVE "TO WRITE TO ANOTHER BRANCH" TO PASSWDW-PROMPT2.
           MOVE 2                            TO PASSWDW-SEQ.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO ENTER-TBRNO1.

      *-----------------------------------------------------------------
       ENTER-TBEG-CL.
           IF CLASS-RANGE = "Y"
              MOVE "S  A020D-BEGCLASS." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020D-ENDCLASS." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-TGR-ID.

           IF NO-CLASS-RANGE
              MOVE "F3-ALL, F6-SCAN, ENTER BEGINNING TARGET CLASS"
                                                               TO MESS
           ELSE
              MOVE "F6-SCAN, ENTER TARGET CLASS" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020D-BEGCLASS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO ENTER-TBRNO1.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "16"
              IF NO-CLASS-RANGE
                 AND NO-GROUP-RANGE
                    AND NO-SEQNO-RANGE
                       GO TO ENTER-TARGET-ALL-CLASS.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF SCAN-ERR-FG = 9
                 GO TO ENTER-TBEG-CL
              ELSE
                 MOVE "S  N020D-BEGCLASS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-TBEG-CL.

           MOVE VDUNUM0 TO D-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-TEND-CL.
           IF CLASS-RANGE = "Y"
              OR GROUP-RANGE = "Y"
                 OR SEQNO-RANGE = "Y"
                    GO TO ENTER-TGR-ID.
           MOVE "F6-SCAN, ENTER ENDING TARGET CLASS" TO MESS
           PERFORM SEND-LEGEND.
           MOVE "ENNN020D-ENDCLASS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO ENTER-TBEG-CL.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF SCAN-ERR-FG = 9
                 GO TO ENTER-TEND-CL
              ELSE
                 MOVE "S  N020D-ENDCLASS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-TEND-CL.

           MOVE VDUNUM0 TO D-END-CLASS
           IF D-END-CLASS < D-BEG-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TEND-CL.

           GO TO ENTER-TGR-ID.
      *-----------------------------------------------------------------
       ENTER-TARGET-ALL-CLASS.
           MOVE "S  N020D-BEGCLASS." TO VDU.
           MOVE 0 TO VDUNUM0 D-BEG-CLASS.
           PERFORM SCREENIO.
           MOVE "S  N020D-ENDCLASS." TO VDU.
           MOVE 99 TO VDUNUM0 D-END-CLASS.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-TGR-ID.
           IF GROUP-RANGE = "Y"
              MOVE "S  A060D-GROUP." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020D-GRPDESC." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-TBEG-SEQNO.

           MOVE "F6-SCAN, ENTER ENDING TARGET GROUP ID" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNA060D-GROUP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              IF NO-CLASS-RANGE
                 GO TO ENTER-TBEG-CL
              ELSE
                 GO TO ENTER-TBRNO1.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1<"
              MOVE HOLD-EA-CQ-FG TO CDSCAN-EA-CQ-FG
              MOVE HOLD-EA-GQ-FG TO CDSCAN-EA-GQ-FG
              PERFORM CODE-SCAN
              IF SCAN-ERR-FG = 9
                 GO TO ENTER-TGR-ID
              ELSE
                 MOVE "S  A060D-GROUP." TO VDU
                 MOVE CDSCAN-CODE TO VDUBUF EA-GROUP-ID
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-TGR-ID.

           MOVE "EA"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE   D-GROUP.

           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              GO TO ENTER-TGR-ID.

      * DISPLAY GROUP DESCRIPTION.
           MOVE "S  A020D-GRPDESC." TO VDU.
           MOVE CD-DESC             TO VDUBUF.
           PERFORM SCREENIO.

           IF CD-EA-CQ-FG NOT = HOLD-EA-CQ-FG
              MOVE "EXTRACTION GROUP ID MISMATCH - EA-CQ-FG" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TGR-ID.

           IF CD-EA-GQ-FG NOT = HOLD-EA-GQ-FG
              MOVE "EXTRACTION GROUP ID MISMATCH - EA-GQ-FG" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TGR-ID.

      *-----------------------------------------------------------------

       ENTER-TBEG-SEQNO.
           IF SEQNO-RANGE = "Y"
              MOVE "S  A020D-BEGSEQNO." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-OVERWRITE.

           MOVE "ENTER ENDING TARGET SEQNO" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020D-BEGSEQNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              IF NO-GROUP-RANGE
                 GO TO ENTER-TGR-ID
              ELSE
                 IF NO-CLASS-RANGE
                    GO TO ENTER-TBEG-CL
                 ELSE
                    GO TO ENTER-TBRNO1.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-TBEG-SEQNO.

           MOVE VDUNUM0 TO D-SEQNO.

      *-----------------------------------------------------------------
       ENTER-OVERWRITE.
           MOVE "OVERWRITE EXISTING EXTRACTIONS ( Y/N )" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNA010OVERWRITE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1U"
              IF NO-SEQNO-RANGE
                 GO TO ENTER-TBEG-SEQNO
              ELSE
                 IF NO-GROUP-RANGE
                    GO TO ENTER-TGR-ID
                 ELSE
                    IF NO-CLASS-RANGE
                       GO TO ENTER-TBEG-CL
                    ELSE
                       GO TO ENTER-TBRNO1.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-OVERWRITE.

           MOVE VDUBUF TO OVERWRITE-FG.
           IF NOT (OVERWRITE-FG = "N" OR "Y")
              GO TO ENTER-OVERWRITE.

           IF OVERWRITE-FG = "Y"
              MOVE "TO OVERWRITE EXTRACTIONS" TO PASSWDW-PROMPT2
              MOVE 1 TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-OVERWRITE.

      *-----------------------------------------------------------------
       ENTER-ACCEPT.
           MOVE "ACCEPT THE ABOVE PARAMETERS ( Y/N )" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNA010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1U" GO TO ENTER-OVERWRITE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-OVERWRITE.

           IF NOT (VDUBUF = "N" OR "Y")
              GO TO ENTER-ACCEPT.
           IF VDUBUF = "N" GO TO ENTER-ACCEPT.


       END-ENTER-RANGES.
           PERFORM RESET-LEGEND.

      *****************************************************************
      *
      *      C O P Y   R E C O R D S
      *
      *****************************************************************
       COPY-RECORDS SECTION.
           MOVE 0 TO WRITE-COUNT REWRITE-COUNT.

           IF GROUP-FLAG = "N"
              MOVE BEG-TARGET-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO EXIT-COPY-RECORDS.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-TARGET-BR
              GO TO EXIT-COPY-RECORDS.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
      * HEY, THIS FLAG MAY BE Y, N OR BLANK SO CHECK CAREFULLY:
           IF (BR-GLOBAL-QUEUE = "Y" AND
               SOURCE-BR-GLOBAL-QUEUE NOT = "Y")
           OR (SOURCE-BR-GLOBAL-QUEUE = "Y" AND
               BR-GLOBAL-QUEUE NOT = "Y")
              MOVE "GLOBAL QUEUING MISMATCH; BRNO &&&& SKIPPED."
                 TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              GO TO NEXT-GROUP-LOC.

           MOVE "N" TO RECORDS-UPDATED.

           MOVE S-BRNO      TO EA-BRANCH
                               QEA1-WBEG-BRANCH
                               QEA1-WEND-BRANCH.
           MOVE S-BEG-CLASS TO EA-CLASS
                               QEA1-WBEG-CLASS.
           MOVE S-END-CLASS TO QEA1-WEND-CLASS.
           MOVE S-BEG-GROUP TO EA-GROUP-ID
                               QEA1-WBEG-GROUP-ID.
           MOVE S-END-GROUP TO QEA1-WEND-GROUP-ID.
           MOVE S-BEG-SEQNO TO EA-SEQNO
                               QEA1-WBEG-SEQNO.
           MOVE S-END-SEQNO TO QEA1-WEND-SEQNO.

           PERFORM START-EA1-FILE.

       NEXT-EA.
           PERFORM READ-EA1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO NEXT-GROUP-LOC.

           IF EA-BRANCH > S-BRNO
              GO TO NEXT-GROUP-LOC.

           IF EA-CLASS < S-BEG-CLASS OR > S-END-CLASS
              GO TO NEXT-EA.

           IF EA-GROUP-ID < S-BEG-GROUP OR > S-END-GROUP
              GO TO NEXT-EA.

           IF EA-SEQNO < S-BEG-SEQNO OR > S-END-SEQNO
              GO TO NEXT-EA.

      * SAVE ORIGINAL EA-REC
           MOVE EA-REC TO EAOR-REC.

      *    -------------------------------------------------------------
      *    IF A TARGET CLASS RANGE IS NOT ENTERED THEN SKIP NEXT-LC LOOP
      *    -------------------------------------------------------------
           MOVE D-BEG-CLASS TO LC-CODE.
           IF CLASS-RANGE = "Y"
              OR GROUP-RANGE = "Y"
                 OR SEQNO-RANGE = "Y"
                     GO TO SKIP-READ-LC.

           PERFORM START-LC1-FILE.

       NEXT-LC.
           PERFORM READ-LC1-FILE-NEXT.
           IF IO-FG = 9
              GO TO NEXT-GROUP-LOC.

           IF LC-CODE > D-END-CLASS
              GO TO NEXT-GROUP-LOC.

       SKIP-READ-LC.

      * SETUP TARGET KEY
           MOVE BR-NO          TO TARGET-EA-BRANCH.

           IF NO-CLASS-RANGE
              MOVE LC-CODE     TO TARGET-EA-CLASS
           ELSE
              MOVE EA-CLASS    TO TARGET-EA-CLASS.

           IF NO-GROUP-RANGE
              MOVE D-GROUP     TO TARGET-EA-GROUP-ID
           ELSE
              MOVE EA-GROUP-ID TO TARGET-EA-GROUP-ID.

           IF NO-SEQNO-RANGE
              MOVE D-SEQNO     TO TARGET-EA-SEQNO
           ELSE
              MOVE EA-SEQNO    TO TARGET-EA-SEQNO.

      * TEST IF TARGET EXISTS

           MOVE TARGET-EA1-KEY TO EA1-KEY.
           PERFORM READ-EA1-FILE.
           IF IO-FG = 9
              GO TO UPDATE-WRITE-EA.

      * IF TARGET EXISTS THEN REWRITE RECORD

           IF OVERWRITE-FG = "N"
              GO TO BOTTOM-OF-LC-LOOP.

           PERFORM COPY-BEGIN.

           MOVE EAOR-REC       TO EA-REC.
           MOVE TARGET-EA1-KEY TO EA1-KEY.
           PERFORM REWRITE-EA1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y" TO RECORDS-UPDATED.

           ADD 1 TO REWRITE-COUNT.

           GO TO BOTTOM-OF-LC-LOOP.

       UPDATE-WRITE-EA.

      * IF TARGET DOES NOT EXIST THEN WRITE NEW RECORD

           PERFORM COPY-BEGIN.
           MOVE EAOR-REC       TO EA-REC.
           MOVE TARGET-EA1-KEY TO EA1-KEY.
           PERFORM WRITE-EA1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y" TO RECORDS-UPDATED.

           ADD 1 TO WRITE-COUNT.

       BOTTOM-OF-LC-LOOP.

      * IF TARGET CLASS IS ALLOWED THEN CONTINUE THROUGH CLASS RANGE
      * ELSE GET NEXT EA RECORD

           IF (NO-CLASS-RANGE) AND (NO-GROUP-RANGE) AND (NO-SEQNO-RANGE)
              GO TO NEXT-LC
           ELSE
      * GET BACK TO RECORD YOU HAD
      *       MOVE EAOR-REC TO EA-REC
      *       PERFORM START-EA1-FILE
      *       PERFORM READ-EA1-FILE-NEXT
              GO TO NEXT-EA.

       EXIT-COPY-RECORDS.
           PERFORM RESET-LEGEND.
           IF RECORDS-WRITTEN
              PERFORM COPY-END
           ELSE
              PERFORM COPY-END2.

      ******************************************************************
      *
      *    C O D E - S C A N
      *
      ******************************************************************
       CODE-SCAN SECTION.
           MOVE 0 TO SCAN-ERR-FG.
           MOVE "EA"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO SCAN-ERR-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO SCAN-ERR-FG.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************************************
      *
      *    C L A S S - S C A N
      *
      ******************************************************************
       CLASS-SCAN SECTION.
           MOVE 0      TO LCSCAN-KEY SCAN-ERR-FG.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO SCAN-ERR-FG
               GO TO EXIT-CLASS-SCAN.
       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
      *
      *                   VERIFY PASSWORD SECTION
      *
      ******************************************************************
       VERIFY-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "EACOPY" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
       EXIT-VERIFY-PASSWORD.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO EACOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EACOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EACOPY-SCREEN UPON EACOPY-WINDOW-HANDLE.
           MOVE EACOPY-FORM-FIELDS TO WR-BUF.
           MOVE EACOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE EACOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE EACOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/EACOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *   M I S C E L L A N O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       RESET-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
      *-----------------------------------------------------------------
       COPY-BEGIN.
           MOVE "COPY IN PROGRESS................" TO MESS.
           PERFORM SEND-LEGEND.
      *-----------------------------------------------------------------
       COPY-END.
           MOVE "COPY COMPLETE, &&&&&& WRITTEN, ###### REWRITTEN."
               TO MESS.
           INSPECT MESS REPLACING FIRST "&&&&&&" BY WRITE-COUNT.
           INSPECT MESS REPLACING FIRST "######" BY REWRITE-COUNT.
           PERFORM SEND-MESS.
       COPY-END2.
           MOVE "NO RECORDS COPIED FOR SELECTED RANGE." TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
      *
      *        F I L E   I / O
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPEA1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **************************************************************
       END-ROUTINE SECTION.
      **************************************************************
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EACOPY-SCREEN.
           DESTROY EACOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
