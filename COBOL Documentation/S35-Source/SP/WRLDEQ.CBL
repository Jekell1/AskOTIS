       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WRLDEQ.
       DATE-WRITTEN.    100804.
      *****************************************************************
      *    DESCRIPTION:  EXTRACTION FOR EQUIFAX FOR SCORING INFO
      *
      * WORLD USES 4/27/2023 (MONTHLY)
      *
      *  DRIVES OFF BORROWER FILE
      *
      * CREATE /USR/DATA/R#/SP/SCORE.YYMMDD   RSZ       INCLUDES CR,LF
      *
      *=================================================================
      * REV:
      * 101101 ADD THE CLASS FOR EXISTING LOANS FOR A GIVEN BORROWER #707
      * 160317 BAH INSTEAD OF INCLUDING NB,FB,DM,ML AND RF, EXCLUDE
      *             PB AND RN, WORLD PR# 1144
      * KEC 2017.0609 CHANGED NEW-CLASS FROM 9(2) TO 9(3).
      * MJD 2017.0911 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.0928 GLOBAL BWFILE
      * BAH 2018.1218 GLOBAL LNFILE
      * BAH 2019.1001 REMOVED ACCESS-CALL LNFILE, BWFILE
      * BAH 2020.0831 REMOVED RESET ON BR1 
      * BAH 2022.0812 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT EQUIFAX-FILE ASSIGN TO EQUIFAX-PATH
                           ORGANIZATION RELATIVE
                           ACCESS DYNAMIC
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           RELATIVE KEY EQUIFAX-KEY
                           FILE STATUS FILE-STAT.



       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      **************************************************
      *  EQUIFAX EXTRACT FILE                 RSZ 121
      **************************************************
       FD  EQUIFAX-FILE
           LABEL RECORDS ARE STANDARD.
       01  EQUIFAX-REC.
           03  EQ-BW-LNAME                 PIC X(16).
           03  EQ-BW-FNAME                 PIC X(16).
           03  EQ-BW-ADR1                  PIC X(24).
           03  EQ-BW-CITY                  PIC X(15).
           03  EQ-BW-STATE                 PIC X(2).
           03  EQ-BW-ZIP                   PIC X(10).
           03  EQ-BW-SSNO                  PIC 9(9).
           03  EQ-BR-NO                    PIC 9(4).
           03  EQ-LN-ACCTNO                PIC 9(6).
           03  EQ-LN-CLASS                 PIC 9(2).
           03  EQ-LN-LOANDATE              PIC 999999.
           03  EQ-LN-SRCD                  PIC X(2).
           03  EQ-BEACON-SCORE             PIC 9(3).
           03  EQ-FACT-ACT-VARIANCE-CD     PIC X(3).
           03  EQ-CRITERIA-CODE            PIC X.

           03  EQ-CR                      PIC X.
           03  EQ-LF                      PIC X.



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/WRLDEQ.CBL S35 02/22/24-07:33:09 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  EQUIFAX-IFN          PIC X(5)     VALUE "EQUIF".
       01  EQUIFAX-KEY          PIC 9(9)     VALUE 0.

       01  EQUIFAX-PATH.
           03  EQUIFAX-PATH-BASE          PIC X(09).
           03  FILLER                     PIC X(01) VALUE "/".
           03  EQUIFAX-PATH-MACHINE       PIC X(02).
           03  FILLER                     PIC X(10) VALUE "/SP/SCORE.".
           03  EQUIFAX-PATH-DATE-YYMMDD   PIC 9(6).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  HOLD-BW-SSNO         PIC 9(9)     VALUE 0.
       01  NEW-NUMBER           PIC 9(8)     VALUE 0.
       01  NEW-LOANDATE         PIC 9(8)     VALUE 0.
       01  NEW-CLASS            PIC 9(3)     VALUE 0.
       01  NEW-SRCD             PIC X(2)     VALUE " ".
       01  NEW-LOAN-ACTIVITY-FG PIC X        VALUE " ".

       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-CREATED        PIC S9(13)  COMP-3 VALUE 0.

       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  FILLER                      REDEFINES LEGEND.
           03  LEGEND-PROMPT           PIC X(20).
           03  LEGEND-BRNO             PIC Z(04).


      ******************************************************************
      *
      *    S C R E E N - R A N G E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BR                  PIC 9(4).
           03  END-BR                  PIC 9(4).
           03  BEG-CL                  PIC 9(3).
           03  END-CL                  PIC 9(3).
           03  EXTRACT-TYPE            PIC X.
           03  BEG-LOANDATE            PIC 9(8).
           03  END-LOANDATE            PIC 9(8).

           03  GROUP-FLAG              PIC X(1).
           03  ENT-GROUP               PIC X(4).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(42) VALUE
           "         EQUIFAX  EXTRACTION          ".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  FILLER           PIC X(2).
           03  RANGE2N          PIC 9(10).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/WRLDEQ_DEF.CPY".
       COPY "LIBSP/WRLDEQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/WRLDEQ_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE EQUIFAX-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               EQUIFAX-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *********************************************
       MAIN-MODULE SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WRLDEQ" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
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
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO BW-PATH-OVERRIDE
                              LN-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES         TO LEGEND
              MOVE "PROCESSING.." TO LEGEND-PROMPT
              MOVE LN-OWNBR       TO LEGEND-BRNO
              PERFORM SEND-LEGEND.

           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           PERFORM START-BW1-FILE.
       NEXT-BW.
           PERFORM READ-BW1-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO END-OF-GROUP.

           MOVE 0 TO NEW-NUMBER
                     NEW-LOANDATE
                     NEW-CLASS
                     NEW-SRCD.

           MOVE "N" TO NEW-LOAN-ACTIVITY-FG.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(1) HOLD-BW-SSNO
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE.

       NEXT-LN.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-SSNO(1) NOT = HOLD-BW-SSNO)
              IF NEW-NUMBER NOT = 0
                 PERFORM CREATE-EQUIFAX-REC
                 GO TO SKIP-LOANS
              ELSE
                 GO TO SKIP-LOANS.

           IF LN-OWNBR > END-BR
              GO TO END-OF-GROUP.

           ADD 1 TO TOTAL-RECORDS.

      * DELETED LOAN, RESCISSION LOANS
           IF LN-POCD-NONLOAN
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CL OR LN-CLASS > END-CL
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      * SKIP CONVERTED ACCOUNTS WITHOUT BALFWD AND PAID OFFS
           IF LN-CURBAL = 0
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      * ALWAYS SKIP TRANSFERRED ACCOUNTS,  THE OPEN NEW ONE WILL GET REPORTED
           IF LN-POCD = "BT"
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      *AT THIS POINT WE HAVE A OPEN NON-PL LOAN.

      *FOR PURPOSES OF THIS EXTRACTION, A BORROWER IS ONLY 'NEW' IF NEW
      *TO WORLD BORROWER/LOAN COMBO. 2 EXTRACT TYPES, 'M' WHICH IS RUN
      *MONTHLY AND ONLY GETS NEW-TO-WORLD-BORROWERS/LOAN. EXTRACT-TYPE 'S'
      *IS SEMI-ANNUAL AND EXTRACTS NEW-TO-WORLD-BORROWERS/LOAN FOR THE
      *MONTH AS WELL AS EXISTING BORROWERS. SO, IF EXTRACT-TYPE 'M' AND
      *LN-SRCD IS NOT ONE OF THESE, GO TEST THE NEXT BORROWER.
      *IF THE EXTRACT TYPE IS 'S', THE BORROWER BELONGS IN THE FILE BUT
      *ISN'T A NEW BORROWER AND I AM CREATING THE EQUIFAX RECORD WITH
      *A ZERO ACCTNO.
           IF (LN-SRCD = "PB" OR "RN")
              IF EXTRACT-TYPE = "M"
                 GO TO SKIP-LOANS
              ELSE
                 MOVE 0 TO NEW-NUMBER NEW-LOANDATE
                 MOVE LN-CLASS TO NEW-CLASS
                 MOVE SPACES TO NEW-SRCD
                 PERFORM CREATE-EQUIFAX-REC
                 GO TO SKIP-LOANS.

      *WE HAVE A NEW SOURCE CODE, CHECK THAT ITS WITHIN THE NEW ACCOUNT
      *ENTRY DATE RANGE.
           PERFORM NB-ACTIVITY-CHECK.

      *IF THE 'NB' IS WITHIN THE DATE RANGE, SAVE THE ACCTNO INFO, BEFORE
      *WRITING TO THE EQUIFAX FILE, VERIFY THE BORROWER DOES NOT HAVE
      *ANY OTHER OPEN NON PL LOANS.
           IF NEW-LOAN-ACTIVITY-FG = "Y"
              MOVE LN-ACCTNO   TO NEW-NUMBER
              MOVE LN-LOANDATE TO NEW-LOANDATE
              MOVE LN-CLASS    TO NEW-CLASS
              MOVE LN-SRCD     TO NEW-SRCD
              GO TO NEXT-LN
           ELSE
           IF EXTRACT-TYPE = "S"
              MOVE 0 TO NEW-NUMBER NEW-LOANDATE
              MOVE LN-CLASS TO NEW-CLASS
              MOVE SPACES TO NEW-SRCD
              PERFORM CREATE-EQUIFAX-REC.

       SKIP-LOANS.
           GO TO NEXT-BW.


       END-OF-GROUP.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-BW1-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.


      *******************************************
       NB-ACTIVITY-CHECK SECTION.
      *******************************************
           MOVE "N" TO NEW-LOAN-ACTIVITY-FG.
           IF LN-LOANDATE < BEG-LOANDATE
              MOVE "N" TO NEW-LOAN-ACTIVITY-FG
           ELSE
              IF LN-LOANDATE > END-LOANDATE
                 MOVE "N" TO NEW-LOAN-ACTIVITY-FG
              ELSE
                 MOVE "Y" TO NEW-LOAN-ACTIVITY-FG.
       NB-ACTIVITY-CHECK-EXIT.
           EXIT.



      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE WRLDEQ-QUEUE-POS TO Q-POS.
           MOVE WRLDEQ-COPIES-POS TO C-POS.
           MOVE WRLDEQ-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE END-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD             TO EQUIFAX-PATH-DATE-YYMMDD.
           MOVE EXT-FILPATH-BASE        TO EQUIFAX-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE     TO EQUIFAX-PATH-MACHINE.
           PERFORM OPEN-EQUIFAX-FILE-OUTPUT.
           PERFORM CLOSE-EQUIFAX-FILE.
           PERFORM OPEN-EQUIFAX-FILE.

           PERFORM OPEN-PR-FILE.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE 0 TO TOTAL-RECORDS TOTAL-BYPASSED TOTAL-CREATED.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
            MOVE EXT-EOBUF-DATE1 TO BEG-LOANDATE.
            MOVE EXT-EOBUF-DATE2 TO END-LOANDATE.


      *******************************************************
       CREATE-EQUIFAX-REC SECTION.
      *******************************************************
           MOVE SPACES TO EQUIFAX-REC.
           MOVE 0 TO EQ-BEACON-SCORE.

      * UNIQUE CUSTOMER ID
           MOVE BW-OWNBR       TO EQ-BR-NO.
           MOVE BW-SSNO        TO EQ-BW-SSNO.
           MOVE BW-LNAME       TO EQ-BW-LNAME.
           MOVE BW-FNAME       TO EQ-BW-FNAME.
           MOVE BW-ADR1        TO EQ-BW-ADR1.
           MOVE BW-CITY        TO EQ-BW-CITY.
           MOVE BW-STATE       TO EQ-BW-STATE.
           MOVE BW-ZIP         TO EQ-BW-ZIP.
           MOVE NEW-NUMBER     TO EQ-LN-ACCTNO.
           MOVE NEW-CLASS      TO EQ-LN-CLASS.
           MOVE NEW-LOANDATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EQ-LN-LOANDATE.
           MOVE NEW-SRCD       TO EQ-LN-SRCD.

           ADD 1               TO TOTAL-CREATED.
           MOVE X"0D" TO EQ-CR.
           MOVE X"0A" TO EQ-LF.

           ADD 1 TO EQUIFAX-KEY.
           PERFORM WRITE-EQUIFAX-FILE.
           IF IO-BAD
              GO TO IO-ERROR.


      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.

           IF ( UP-KEY )
             GO TO EXTRACT-TYP.

      *-----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO ENTER-PARAMETERS-EXIT.
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
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS = "10" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL > END-CL
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO EXTRACT-TYP.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.

       EXTRACT-TYP.

           MOVE "E  A010TYP." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY   GO TO CL-01.
           IF UP-KEY   GO TO CL-02.
           IF F7-KEY   GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO EXTRACT-TYP.

           MOVE VDUBUF TO EXTRACT-TYPE.
           IF NOT (EXTRACT-TYPE = "M" OR "S")
              MOVE "INVALID EXTRACT TYPE" TO MESS
              PERFORM SEND-MESS
              GO TO EXTRACT-TYP.



       LOANDATE-1.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY  GO TO ENTER-PARAMETERS-EXIT.
           IF F1-KEY  GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY  GO TO EXTRACT-TYP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LOANDATE-1.

           MOVE VDU-DATE-YYYYMMDD TO BEG-LOANDATE.


       LOANDATE-2.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY  GO TO ENTER-PARAMETERS-EXIT.
           IF F1-KEY  GO TO ENTER-PARAMETERS-EXIT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LOANDATE-2.

           MOVE VDU-DATE-YYYYMMDD TO END-LOANDATE.

           IF END-LOANDATE < BEG-LOANDATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO LOANDATE-1.

           GO TO ENTER-PARAMETERS-EXIT.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CL TO VDUNUM0.
           PERFORM SCREENIO.


      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CL - END CL" TO RANGE-SELECTION.
           MOVE BEG-CL TO RANGE1N.
           MOVE END-CL TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS EXTRACTED" TO RANGE-SELECTION.
           MOVE TOTAL-CREATED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.


      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO WRLDEQ-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WRLDEQ-SYSDATE.
           DISPLAY WRLDEQ-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WRLDEQ-FORM-FIELDS TO WR-BUF.
           MOVE WRLDEQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE WRLDEQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRLDEQ-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WRLDEQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************
       IO-ROUTINES SECTION.
      *************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       OPEN-EQUIFAX-FILE.
           PERFORM OPEN-IT.
           MOVE EQUIFAX-IFN TO E-FILE.
           OPEN I-O EQUIFAX-FILE.
           IF IO-FG = 8
              GO TO OPEN-EQUIFAX-FILE.
           IF IO-FG = 7
              CLOSE EQUIFAX-FILE
              GO TO OPEN-EQUIFAX-FILE.
       OPEN-EQUIFAX-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE EQUIFAX-IFN TO E-FILE.
           OPEN OUTPUT EQUIFAX-FILE.
           IF IO-FG = 8
              GO TO OPEN-EQUIFAX-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE EQUIFAX-FILE
              GO TO OPEN-EQUIFAX-FILE-OUTPUT.
       WRITE-EQUIFAX-FILE.
           PERFORM WRITE-IT.
           MOVE EQUIFAX-IFN TO E-FILE.
           MOVE EQUIFAX-KEY TO E-KEYX.
           WRITE EQUIFAX-REC.
           IF IO-FG = 8
              GO TO WRITE-EQUIFAX-FILE.
       CLOSE-EQUIFAX-FILE.
           PERFORM CLOSE-IT.
           CLOSE EQUIFAX-FILE.

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *************************
       END-PROGRAM SECTION.
      *************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY WRLDEQ-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
