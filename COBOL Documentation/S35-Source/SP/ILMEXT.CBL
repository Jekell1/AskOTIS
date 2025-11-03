       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ILMEXT.
       DATE-WRITTEN.    09/13/2022.
      *********************************************************************
      *
      *    CREATE ASCII FILE FOR ILLINOIS SMALL LOAN REPORTING
      *           ** MISPAY UPDATES VERITEC **
      *
      * NOTE: SP/MISPAY WILL BE RAN NIGHTLY TO SET LSM-MISPAY-90
      *
      *    WRITES FILE  "../../FTP/ILL/WFCILM.YYMMDD",  PIPE DELIMITED
      *    DRIVES OFF LOAN MASTER
      *    ALL CLASSES EXCEPT 50 AND 55
      *    ONLY INCLUDES LN-ORGST = "IL"
      *    SKIPS VOIDS
      *    LOANDATE CAN NOT BE PRIOR TO 8/1/22
      *    LS-M-MISPAY-90-DATE + 3MO  MUST BE IN THE DATE RANGE
      *
      * VERITEC NOTED THE UNIQUE LOAN ID IS NOT REQUIRED, ONLY THE
      *   TRANSACTION ID (VERITEC-TRANS-ID)
      *
      *  REV:
      *  20220907 BAH NEW #1570
      *  20240702 SKIP CLASS 55 #1662 S35Q-75
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSILLINOIS.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBSP/SPFDILLINOIS.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/ILMEXT.CBL S35 09/23/22-08:12:18 >".

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
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBSP/SPWSILLINOIS.CPY".
       COPY "LIBSP/ILLPATH.CPY".

      *----------------------------------------------------------------

       01  RECORD-COUNT          PIC 9(9)   VALUE 0.
       01  JUSTIFY-LEFT          PIC X      VALUE "L".
       01  VERITEC-TRANS-ID      PIC X(12).

      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(31).

      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR             PIC 9(4).
           03  END-BR             PIC 9(4).
           03  BEG-DATE           PIC 9(8).
           03  END-DATE           PIC 9(8).
           03  GROUP-FLAG         PIC X.
           03  ENT-GROUP          PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *     P R I N T   B U F F E R S
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(3)     VALUE " ".
           03  H-TITLE          PIC X(50)    VALUE
           "ILLINOIS SMALL LOAN MISPAY UPDATE EXTRACTION".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE-PATH.
               05  RANGE1       PIC X(10).
               05  RANGE1N REDEFINES RANGE1 PIC Z(9)9.
               05  RANGE1D REDEFINES RANGE1 PIC 99/99/99BB.
               05  FILLER       PIC X(2).
               05  RANGE2       PIC X(10).
               05  RANGE2N REDEFINES RANGE2 PIC Z(9)9.
               05  RANGE2D REDEFINES RANGE2 PIC 99/99/99BB.
               05  FILLER       PIC X(20).

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ILMEXT_DEF.CPY".
       COPY "LIBSP/ILMEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ILMEXT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE ILLINOIS-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-ILLINOIS-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ILMEXT" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
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

      D    STOP MESS.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              LSM-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.
 
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LSM1-FILE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ****************************************
       GLOBAL-UPDATE SECTION.
      ****************************************

           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-BRNO
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LSM1-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE 0         TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE ALL "9"   TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.
           
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-GLOBAL-UPDATE.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.

           IF LN-ORGST NOT = "IL"
              GO TO NEXT-LN.

           IF LN-CLASS = 50 OR 55
              GO TO NEXT-LN.

      * THIS NEW PROGRAM START WITH ALL LOANS MADE ON OR AFTER 8/1/22
           MOVE LN-LOANDATE   TO SYS-DATE.
           IF LN-CONVERCD = "Y"
              MOVE LN-ENTDATE TO SYS-DATE.
           IF SYS-DATE < 20220801
              GO TO NEXT-LN.

      * GET VERITEC ID AND 90 DAY MISPAY DATE OF SCHEDULED PAYMENT
           MOVE LN-OWNBR  TO LSM-BRNO.
           MOVE LN-ACCTNO TO LSM-ACCTNO.
           MOVE 1         TO LSM-SEQNO.
           PERFORM READ-LSM1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-LN.

           MOVE LSM-MISC-FIELD(15)    TO VERITEC-TRANS-ID.

      * THIS DATE RANGE IS FOR THE RANGE OF 90 MISPAY
      * NEED TO ADD 90 DAYS(3 MONTHS) TO THE LSM-MISPAY-90-DAY AS THAT IS THE
      * DATE OF THE SCHEDULED PAYMENT THAT IS 90 DAY PAST DUE AND ALSO
      * THE EVENT DATE THAT MUST GET REPORTED

      * USING 90 DAYS DIDNT LAND ON A PAY CYCLE, NEED TO USE MONTHS.
           MOVE 3                   TO NDTE-HOLD.
           MOVE LSM-MISPAY-90-DATE  TO NDTE-DATE.
           MOVE LN-UNITPER-CD       TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ     TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.

           IF NDTE-DATE < BEG-DATE
              GO TO NEXT-LN.

           IF NDTE-DATE > END-DATE
              GO TO NEXT-LN.

           PERFORM CREATE-UPDATE-RECORD.

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE ILMEXT-QUEUE-POS TO Q-POS.
           MOVE ILMEXT-COPIES-POS TO C-POS.
           MOVE ILMEXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BR TO SV-BEG-BR
              MOVE END-BR TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BR = 0 AND END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           CALL "C$TOLOWER" USING ILM-FTP-PATH, VALUE 9.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD-DD TO ILM-PATH-DD.
           MOVE DATE-YYYYMMDD-MM TO ILM-PATH-MM.
           MOVE DATE-YYYYMMDD-YY TO ILM-PATH-YY.

           PERFORM LOAD-ILM-FILE.
           MOVE ILM-PATH TO ACCESS-BUF ILLINOIS-PATH.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO CONTINUE-INITIALIZE.

           IF EXT-ROUTE-BUF NOT = "00"
              GO TO CONTINUE-INITIALIZE.
       OVERWRITE-QUESTION.
           MOVE "ILM FILE EXISTS FOR THIS DATE. CONTINUE? (Y/N)"
                           TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A000ANS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" GO TO OVERWRITE-QUESTION.
           IF VDUBUF = "Y" GO TO CONTINUE-INITIALIZE.
           IF VDUBUF NOT = "N"
              GO TO OVERWRITE-QUESTION.
           GO TO ENDIT.

       CONTINUE-INITIALIZE.
           PERFORM OPEN-ILLINOIS-FILE-OUTPUT.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.
      * BEGIN DATE CAN NOT BE PRIOR TO 8/1/22
           IF EXT-EOBUF-DATE1 < 20220801
              MOVE 20220801 TO BEG-DATE
              MOVE 20220831 TO END-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF NOT (EXT-ROUTE-BUF = "40" OR "50")
                 GO TO DATE-02.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
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
                 MOVE "S NA000BR1." TO VDU
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
              MOVE 0    TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO DATE-01.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO DATE-01.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       DATE-01.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      * ONLY GET LOANS 8/1/22 AND BEYOND
           IF BEG-DATE < 20220801
              MOVE "S  A080BEGDATE." TO VDU
              MOVE 20220801          TO BEG-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
              MOVE BEG-DATE        TO VDUNUM0.

      * DEFAULT END-DATE TO LAST DAY OF MONTH FROM BEG-DATE
           MOVE BEG-DATE TO NDTE-DATE.
           MOVE 66       TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE "S  A080ENDDATE." TO VDU.
           MOVE NDTE-DATE TO END-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-01.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF BEG-DATE NOT = 0
              IF END-DATE < BEG-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO DATE-02.

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
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

      ***********************************
       CREATE-UPDATE-RECORD SECTION.
      ***********************************

           MOVE SPACES TO ILLINOIS-FIELDS.

           MOVE "UPDATE"             TO ILN-ACTION.
           MOVE BR-VERITEC-LICENSE   TO ILN-LICENSE-ID.

      * VERITEC NOTED THE UNIQUE LOAN ID IS NOT REQUIRED, ONLY THE
      *   TRANSACTION ID (VERITEC-TRANS-ID), STILL USING THOUGH
           MOVE LN-OWNBR             TO ILN-LENDER-BRNO.
           MOVE LN-ACCTNO            TO ILN-ACCTNO.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE BW-SSNO            TO ILN-SSNO(1).
           MOVE "SSN"              TO ILN-ID-TYPE(1).
           MOVE BW-FNAME           TO ILN-FNAME(1).
           MOVE BW-LNAME           TO ILN-LNAME(1).
           MOVE BW-ADR1            TO ILN-ADR1(1).
           MOVE BW-ADR2            TO ILN-ADR2(1).
           MOVE BW-CITY            TO ILN-CITY(1).
           MOVE BW-STATE           TO ILN-STATE(1).
           MOVE BW-ZIP             TO ILN-ZIP(1).

      * VERITEC NOTED THE UNIQUE LOAN ID IS NOT REQUIRED, ONLY THE
      *   TRANSACTION ID (VERITEC-TRANS-ID)

      * THIS FIELD WILL IDENTIFY THE ACCOUNT EVEN IF TRANFERRED. IT
      * WILL STAY WITH THE ACCOUNT
           MOVE VERITEC-TRANS-ID TO ILU-TRANS-ID.

      * MISSED PAYMENTS
           MOVE "MP"                TO ILU-EVENT.
           MOVE LSM-MISPAY-90-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY       TO ILU-EVENT-DATE.

      * THE STRINGS ARE SPLIT UP INTO SECTIONS JUST TO MAKE IT SAFER.

           MOVE SPACES TO ILN-HEADER-INFO.
           STRING ILN-ACTION, "|",
                  ILN-LICENSE-ID, "|",
                  ILN-LENDER-ID, "|",
                  ILN-CUST-COUNT, "|"
              DELIMITED BY "  " INTO ILN-HEADER-INFO.

           MOVE SPACES TO ILN-CUST-INFO.
      * COULDNT GET A PERFORM VARYING TO WORK HERE.
           STRING ILN-SSNO(1), "|",
                  ILN-ID-TYPE(1), "|",
                  ILN-FNAME(1), "|",
                  ILN-LNAME(1), "|",
                  ILN-ADR1(1), "|",
                  ILN-ADR2(1), "|",
                  ILN-CITY(1), "|",
                  ILN-STATE(1), "|",
                  ILN-ZIP(1), "|",

                  ILN-SSNO(2), "|",
                  ILN-ID-TYPE(2), "|",
                  ILN-FNAME(2), "|",
                  ILN-LNAME(2), "|",
                  ILN-ADR1(2), "|",
                  ILN-ADR2(2), "|",
                  ILN-CITY(2), "|",
                  ILN-STATE(2), "|",
                  ILN-ZIP(2), "|",

                  ILN-SSNO(3), "|",
                  ILN-ID-TYPE(3), "|",
                  ILN-FNAME(3), "|",
                  ILN-LNAME(3), "|",
                  ILN-ADR1(3), "|",
                  ILN-ADR2(3), "|",
                  ILN-CITY(3), "|",
                  ILN-STATE(3), "|",
                  ILN-ZIP(3), "|",

                  ILN-SSNO(4), "|",
                  ILN-ID-TYPE(4), "|",
                  ILN-FNAME(4), "|",
                  ILN-LNAME(4), "|",
                  ILN-ADR1(4), "|",
                  ILN-ADR2(4), "|",
                  ILN-CITY(4), "|",
                  ILN-STATE(4), "|",
                  ILN-ZIP(4), "|"
              DELIMITED BY "  " INTO ILN-CUST-INFO.

           MOVE SPACES TO ILN-LOAN-INFO.
           STRING ILN-LOAN-TYPE, "|",
                  ILN-AGREEMENT-DATE, "|",
                  ILN-MATURITY-DATE, "|",
                  ILN-PRINCIPAL-AMOUNT, "|",
                  ILN-TOTAL-AMOUNT, "|",
                  ILN-INTEREST-TYPE, "|",
                  ILN-NUMBER-OF-PAYS, "|",
                  ILN-SCHEDULED-PAYMENT, "|",
                  ILN-SECURITY, "|",
                  ILN-SECURITY-DESC, "|",
                  ILN-APRATE, "|",
                  ILN-PLPA-APRATE, "|",
                  ILN-PAYOFF, "|",
                  ILN-VIN, "|"
              DELIMITED BY "  " INTO ILN-LOAN-INFO.

           MOVE SPACES TO ILU-UPDATE-INFO.
           STRING ILU-TRANS-ID, "|",
                  ILU-EVENT, "|",
                  ILU-EVENT-DATE, "|",
                  ILU-FEE-AMOUNT, "|",
                  ILU-DUE-AMOUNT, "|",
                  ILU-TOTAL-PAID, "|",
                  ILU-CLOSE-REASON, "|",
                  ILU-CLOSE-DESC, "|",
                  ILU-DEBT-SOLD, "|",
                  ILU-DEBT-CONTINUING, "|",
                  ILU-SURPLUS-DEFICIENCY, "|",
                  ILU-DEFICIENCY-WAIVED, "|",
                  ILU-NSF-FEE
              DELIMITED BY "  " INTO ILU-UPDATE-INFO.

           MOVE SPACES TO ILLINOIS-REC.

           STRING ILN-HEADER-INFO,
                  ILN-CUST-INFO,
                  ILN-LOAN-INFO,
                  ILU-UPDATE-INFO,
                  ILN-CARRAGE-RET
              DELIMITED BY "  " INTO ILLINOIS-REC.

           PERFORM WRITE-ILLINOIS-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           ADD 1    TO RECORD-COUNT.
           MOVE "N" TO FIRST-TIME.
 
       EXIT-CREATE-UPDATE-RECORD.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG - END LOAN DATE" TO RANGE-SELECTION.
           MOVE BEG-DATE TO RANGE1D.
           MOVE END-DATE TO RANGE2D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "START - END TIME"     TO RANGE-SELECTION.
           MOVE STARTING-TIME          TO RANGE1.
           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OUTPUT FILE PATH" TO RANGE-SELECTION.
           MOVE ILM-PATH TO RANGE-PATH.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
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
        EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ****************************
       HEAD-PAGE SECTION.
      ****************************
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


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO ILMEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ILMEXT-SYSDATE.
           DISPLAY ILMEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ILMEXT-FORM-FIELDS TO WR-BUF.
           MOVE ILMEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE ILMEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE ILMEXT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ILMEXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************
       MISC-ROUTINES SECTION.
      *****************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *****************************
       IO-ROUTINE SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLSM1RN.CPY".
       COPY "LIBSP/SPILLINOISIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ILMEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
