       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PLTREX.
       DATE-WRITTEN. 022516.
      ******************************************************************
      *
      *                      (SP)
      *    DESCRIPTION:  EXTRACT CHARGE-OFFS FROM TRFILE
      *
      *                  WILL CREATE TAB-DELIMITED FILE IN
      *                  /USR/EXTRACT/ACT/NETCHGOFF
      *
      * REV:
      * BLM 160225 NEW, WORLD PR# 1141
      * BLM 160303 ADDED TR MO & YR TO WK2-KEY TO STOP DUPS SINCE WE ARE 
      *            CROSSING MONTHS, WORLD PR# 1141A
      * BLM 160621 FIX PROBLEM IF BRANCH DIDN'T HAVE TRFILE FOR BEG PL MONTH,
      *            IT WAS NOT INCLUDED.  WORLD PR# 1164
      * 20171012 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018.0424 REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      * BAH 181217 GLOBAL LNFILE
      * BAH 20190725 SEVERAL ISSUES, FIRST NO RECORDS WERE FOUND, LOADS
      *              WERE NOT DONE PRIOR TO ACCESS CALL, WK2-PATH WAS NOT
      *              DEFINED BUT IT COMPILED, WK2-PATH HAD MKTEMP TWICE AND
      *              ONE WAS OPEN OUTPUT AND SECOND ONE WAS PLAIN OPEN QA0063
      * BAH 20191003 REMOVED ACCESS-CALL LNFILE, TRP
      * BAH 20200427 ADDED SOURCE CODE & CLASS BETWEEN ACCTNO & LOANDATE #1431
      * BLM 20210810 REMOVE *P-CODE, TP-POOLID NO LONGER IN KEY
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
				  
           SELECT CHGOFF-FILE ASSIGN OUTPUT CHGOFF-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *  INDEXED TRFILE BY BRANCH, CLASS, ACCOUNT
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH            PIC 9(4).
      * LEAVE THIS 9(2) UNTIL WE HAVE APPROVAL TO EXPAND IT EVERYWHERE. NEED
      * TO MATCH A15
               05  WK2-CLASS             PIC 9(2).
               05  WK2-ACCTNO            PIC 9(6).
               05  WK2-DATE              PIC 9(8).
               05  WK2-LINE              PIC 9(3).
           03  WK2-FIELDS.
               05  WK2-LOANDATE          PIC 99/99/9999.
               05  WK2-LNAMT             PIC ZZZZZ9.99.
               05  WK2-PL-TRDATE         PIC 99/99/9999.
               05  WK2-PLDATE            PIC 99/99/9999.
               05  WK2-TRCD              PIC XX.
               05  WK2-REFNO             PIC X(5).
               05  WK2-TRAMT             PIC -------9.99.
               05  WK2-WK-AMT            PIC -------9.99 OCCURS 6.
               05  WK2-NET-LOSS          PIC -------9.99.
               05  WK2-SRCD              PIC XX.
               
      *****************************************************************
      *    CHGOFF EXTRACT FILE
      *****************************************************************
       FD  CHGOFF-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 500 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  CHGOFF-REC.
           03  CHGOFF-BYTE             PIC X OCCURS 200 TIMES.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/PLTREX.CBL S35 03/25/24-15:36:01 >".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK2-PATH             PIC X(35).
      
       01  CHGOFF-PATH.
           03  CHGOFF-PATH1.
               05  CHGOFF-PATH1A PIC X(12) VALUE "/USR/EXTRACT".
               05  FILLER        PIC X(5) VALUE "/ACT/".
           03  CHGOFF-PATH2      PIC X(9) VALUE "NETCHGOFF".
 
      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
      ******************************************************************
      *
      *    M I S C - W O R K E R S
      *
      ******************************************************************
       01  SUB                  PIC 9(2)      COMP-3.
       01  SUB1                 PIC 9(1)      COMP-3.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
 
       01  LEGEND               PIC X(70).
       01  EXITPROG             PIC X(9)        VALUE "SP/EXTRMU".
     
       01  HOLD-NET-LOSS        PIC S9(9)V99      COMP-3.
       01  TRUE-NET-LOSS        PIC S9(9)V99      COMP-3.
       01  GR-TOT-EXTRACTED     PIC 9(9)   VALUE 0.

       COPY "LIBLP/EMSFRG_CMD.CPY".

      ******************************************************************
      *
      *    S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH              PIC 9(4).
           03  END-BRANCH              PIC 9(4).
           03  BEG-LN-DATE             PIC 9(8).
	   03  END-LN-DATE             PIC 9(8).
	   03  BEG-PL-DATE             PIC 9(8).
	   03  END-PL-DATE             PIC 9(8).
           03  GROUP-FLAG              PIC X.
           03  ENT-GROUP               PIC X(4).
           03  PL-TYPE                 PIC X(1).

       01  VERSION-CONTROL             PIC X      VALUE "A".

       01  SV-BEG-BR                   PIC 9(4)   VALUE 9999.
       01  SV-END-BR                   PIC 9(4)   VALUE 9999.
       01  SV-GROUP-FLAG               PIC X     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(4)  VALUE SPACES.
 
      ******************************************************************
      *
      *     P R I N T - B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.
 
      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(32)    VALUE " ".
           03  FILLER           PIC X(39) VALUE
               "CHARGE-OFF EXTRACTION".
 
      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER            PIC X(20)  VALUE
           "BRNO   TOT EXTRACTED".
      *-----------------------------------------------------------------

       01  DETAIL-LINE          PIC X(132).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(9)9.
               04  R-NUM-REDEF  REDEFINES R-NUM.
                   05  R-CHAR   PIC X(10).
               04  FILLER       PIC X(48).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PLTREX_DEF.CPY".
       COPY "LIBSP/PLTREX_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GETPRW.CPY".
  
      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/PLTREX_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK2-FILE CHGOFF-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK2-FILE CHGOFF-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PLTREX" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.

           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              TRP-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

       NEXT-TR-EXTRACT.

           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           CALL "C$JUSTIFY" USING WK2-LNAMT, "L".
           CALL "C$JUSTIFY" USING WK2-TRAMT, "L".
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              CALL "C$JUSTIFY" USING WK2-WK-AMT(SUB), "L"
           END-PERFORM.
           CALL "C$JUSTIFY" USING WK2-NET-LOSS, "L".

           MOVE SPACES TO CHGOFF-REC.
           STRING WK2-BRANCH, "|",
                  WK2-ACCTNO, "|",
                  WK2-SRCD, "|",
                  WK2-CLASS, "|",
                  WK2-LOANDATE, "|",
                  WK2-LNAMT, "|",
                  WK2-PL-TRDATE, "|",
                  WK2-PLDATE, "|",
                  WK2-TRCD, "|",
                  WK2-REFNO, "|",
                  WK2-TRAMT, "|",
                  WK2-WK-AMT(1), "|",
                  WK2-WK-AMT(2), "|",
                  WK2-WK-AMT(3), "|",
                  WK2-WK-AMT(4), "|",
                  WK2-WK-AMT(5), "|",
                  WK2-WK-AMT(6), "|",
                  WK2-NET-LOSS, "|"
               DELIMITED BY " " INTO CHGOFF-REC.

           PERFORM WRITE-CHGOFF-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           ADD 1 TO GR-TOT-EXTRACTED.

           MOVE "Y" TO RECORDS-FOUND.
	   MOVE "Y" TO RECORDS-FOUND.
	   MOVE "Y" TO RECORDS-FOUND.
               
           GO TO NEXT-TR-EXTRACT.

       END-OF-GROUP.

           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-WK2-FILE.

           PERFORM OPEN-WK2-FILE-OUTPUT.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
 
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE PLTREX-QUEUE-POS TO Q-POS.
           MOVE PLTREX-COPIES-POS TO C-POS.
           MOVE PLTREX-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF BEG-PL-DATE = 0
              MOVE 19000101 TO BEG-PL-DATE.
           IF END-PL-DATE = 0
              MOVE 19000101 TO END-PL-DATE.
           

           MOVE SPACES TO RECORDS-FOUND.
           MOVE 0      TO GR-TOT-EXTRACTED.

      * CREATE INDEXED KEY FILE FOR TRFILE OUTPUT

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF ( STAT NOT = "00" )
              MOVE "CANNOT MAKE WK2 SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           
           CALL "C$TOLOWER" USING CHGOFF-PATH1A, VALUE 12.
           CALL "C$TOLOWER" USING CHGOFF-PATH2, VALUE 9.
           MOVE CHGOFF-PATH1 TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT-BAD
              CALL "C$MAKEDIR" USING ACCESS-BUF GIVING STAT-99.
           PERFORM OPEN-CHGOFF-FILE-OUTPUT.
              
           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           MOVE "REPORT IN PROGRESS............" TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF ( GROUP-FLAG = "Y" )
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-PL-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-PL-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-PL-TYPE.

      *----------------------------------------------------------------
       BR-01.

           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.

           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.

           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.
              

      *----------------------------------------------------------------
       ENTER-BEG-LN-DATE.
           MOVE "ERNM060LNDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-LN-DATES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BEG-LN-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LN-DATE.              
      *----------------------------------------------------------------
       ENTER-END-LN-DATE.
           MOVE "ERNM060LNDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-BEG-LN-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-END-LN-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-LN-DATE.

           GO TO ENTER-BEG-PL-DATE.
      *----------------------------------------------------------------
       ALL-LN-DATES.
           MOVE "S  8080LNDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080LNDATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.  
      *----------------------------------------------------------------
       ENTER-BEG-PL-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-PL-TYPE.
           MOVE "ERNM060PLDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-END-LN-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BEG-PL-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-PL-DATE.              
      *----------------------------------------------------------------
       ENTER-END-PL-DATE.
           MOVE "ERNM060PLDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-BEG-PL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-END-PL-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-PL-DATE.
           
      *----------------------------------------------------------------
       ENTER-PL-TYPE.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-END-PL-DATE.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PL-TYPE.

           MOVE VDUBUF TO PL-TYPE.

           IF NOT (PL-TYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO ENTER-PL-TYPE.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
 
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

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
      
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      
           MOVE "S  8080LNDATE1." TO VDU.
           MOVE BEG-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080LNDATE2." TO VDU.
           MOVE END-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      
           MOVE "S  8080PLDATE1." TO VDU.
           MOVE BEG-PL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080PLDATE2." TO VDU.
           MOVE END-PL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PL-TYPE     TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       SORT-ROUTINE SECTION.
      ******************************************************************

           PERFORM OPEN-TRP1-FILE.


           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEG-PL-DATE         TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE END-PL-DATE         TO  SQL-DATE-YYYYMMDD.
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

       SORT-TR-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-SORT-ROUTINE.

           IF TP-DATE > END-PL-DATE
              GO TO EXIT-SORT-ROUTINE.

           IF NOT (TP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5" 
                              OR "PX")
              GO TO SORT-TR-NEXT.

           IF PL-TYPE = SPACES
              MOVE "A" TO PL-TYPE.

           IF PL-TYPE NOT = "A"
              IF PL-TYPE NOT = "P"
      * 2 - 5
                 IF PL-TYPE NOT = TP-TRCD2
                    GO TO SORT-TR-NEXT
      * P (PL)
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF TP-TRCD2 NOT = "L"
                    GO TO SORT-TR-NEXT.

           MOVE TP-BRNO   TO LN-OWNBR.
           MOVE TP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO SORT-TR-NEXT.
                    
           IF LN-LOANDATE < BEG-LN-DATE OR LN-LOANDATE > END-LN-DATE
              GO TO SORT-TR-NEXT.
           
           MOVE TP-DATE            TO WK2-DATE.
           MOVE TP-BRNO            TO WK2-BRANCH.
           MOVE TP-CLASS           TO WK2-CLASS.
           MOVE TP-ACCTNO          TO WK2-ACCTNO.
           MOVE TP-LINE            TO WK2-LINE.
           
	   MOVE LN-LOANDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
	   MOVE DATE-MMDDYYYY      TO WK2-LOANDATE.
           MOVE LN-LNAMT           TO WK2-LNAMT.
	   MOVE TP-DATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
	   MOVE DATE-MMDDYYYY      TO WK2-PL-TRDATE.
	   MOVE TP-PAYDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
	   MOVE DATE-MMDDYYYY      TO WK2-PLDATE.

           MOVE TP-TRCD            TO WK2-TRCD.
           MOVE TP-REFNO           TO WK2-REFNO.
           MOVE TP-TRAMT           TO WK2-TRAMT.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE TP-WORKAMT(SUB) TO WK2-WK-AMT(SUB)
           END-PERFORM.
           
           MOVE 0 TO HOLD-NET-LOSS
                     TRUE-NET-LOSS.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF SUB1 NOT = 4
                  ADD TP-WORKAMT(SUB1) TO HOLD-NET-LOSS
              END-IF
              IF NOT( GP-INCL-OTH-IN-EARNED = "R") AND SUB1 = 4
                  ADD TP-WORKAMT(4) TO HOLD-NET-LOSS
              END-IF
           END-PERFORM.
           SUBTRACT HOLD-NET-LOSS FROM TP-TRAMT GIVING TRUE-NET-LOSS.
           MOVE TRUE-NET-LOSS      TO WK2-NET-LOSS.
           MOVE LN-SRCD            TO WK2-SRCD.
                     
           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       EXIT-SORT-ROUTINE.
           PERFORM CLOSE-WK2-FILE.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 99 TO LN-FEED.
           
           MOVE "GRAND TOTAL EXTRACTED:" TO RANGE-LINE.
           MOVE GR-TOT-EXTRACTED TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           
           MOVE "OUTPUT FILE PATHNAME :" TO RANGE-LINE.
           MOVE CHGOFF-PATH TO RANGE-SELECTION.
	   MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

	   MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG LOAN DATE" TO RANGE-LINE.
           MOVE BEG-LN-DATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN DATE" TO RANGE-LINE.
           MOVE END-LN-DATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG P&L DATE" TO RANGE-LINE.
           MOVE BEG-PL-DATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END P&L DATE" TO RANGE-LINE.
           MOVE END-PL-DATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "P&L TYPE" TO RANGE-LINE.
           MOVE PL-TYPE   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO PLTREX-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PLTREX-SYSDATE.
           DISPLAY PLTREX-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PLTREX-FORM-FIELDS TO WR-BUF.
           MOVE PLTREX-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE PLTREX-HELP--FILE TO HELP-LINK-FILE.
           MOVE PLTREX-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PLTREX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MSEL-LIST TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

 
      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
      *-----------------------------------------------------------------
       OPEN-CHGOFF-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE CHGOFF-PATH TO E-FILE.
           OPEN OUTPUT CHGOFF-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-CHGOFF-FILE-OUTPUT.
           IF ( IO-FG = 7 )
              CLOSE CHGOFF-FILE
              GO TO OPEN-CHGOFF-FILE-OUTPUT.
 
      *-----------------------------------------------------------------
       WRITE-CHGOFF-FILE.
           PERFORM WRITE-IT.
           MOVE CHGOFF-PATH TO E-FILE.
           WRITE CHGOFF-REC.
           IF ( IO-FG = 8 )
              GO TO WRITE-CHGOFF-FILE.
      *-----------------------------------------------------------------
       CLOSE-CHGOFF-FILE.
           PERFORM CLOSE-IT.
           CLOSE CHGOFF-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
 
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY PLTREX-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
