       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TITLEW.
      ******************************************************************
      *   PAYOFF TITLE PROCESSING   F9-COLLATERAL
      *                             PAGE 3
      *                             (WINDOW)
      * REV:
      *  040412 BAH NEW PAYOFF TITLE PROCESSING REGACC PR# 53
      *  040803 BAH ADDED LT-L-SEND-ASAP REGACC PR# 58
      *  080708  CS ADDED LT-L-TITLE-SEND-FMNO. IF POSTING(PO OR IN) WRITE
      *             LIENHOLDER TO 'SEND TITLE TO'. IF NO LIENHOLDER WALK
      *             THRU FIELDS 2-6 FORCING ENTRY OF NAME,ADR,CITY,ST,ZIP
      *             (POSTING ONLY)... I FORCED NEW-FLAG
      *             IF NOT POSTING(PO/IN) DO NOT UPDATE 'SEND TITLE TO' AND
      *             SET NEW FLAG TO MODIFY  REGACC PR#187
      *  080722  CS IF GP-I-AM-REGACC FORCE CORPORATE PRINTING BY SETTING UP
      *             FM-PATH
      *  080722  CS WRITE THE LM-MEMO WITH THE NEW INFO PR#187
      *  080729  CS ADDED LR-FILE TO DECLARATIVES PR#187
      *  100820 BAH ADDED LT-L-TITLE-ADR2 FOR REGACC, PR# 270
      *  120905 BAH ADDED LT-L-RELEASE-TYPE AND SETTING OF IT FOR REGACC PR# 320
      *  120928 BAH ADDED LT-L-CARRIER-TYPE, LT-L-CARRIER-ACCTNO REGACC PR# 322
      *  140505 BAH RENAMED WI/TITLE TO WI/TITLEW, RESERVED WORD VERYANT
      *  150803 BAH USE HOLD-SYS-DATE TO LM-DATE, NOT SYS-DATE, GETTING BAD
      *             DATES BETWEEN TIME SYS-DATE IS SET! PCCREDIT PL#884
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *         ****NOTE**** D-SEND-FMNO STILL 3 BYTES (SCREEN EDIT).
      *  BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      *  BAH 170424 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181116 GLOBAL FMFILE
      *  BAH 181130 GLOBAL LMFILE
      *  BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *             REDEFINE IN THE FM-REC
      *  BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      *  BAH 20191007 REMOVED ACCESS-CALL FMFILE
      * BAH 2024.0201 REMOVED NOTICE FOR A FORM REQUEST
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/TITLEW.CBL S35 02/22/24-12:50:15 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".


       01  ERRCD                       PIC X VALUE " ".
       01  SAVE-LM1-KEY                 VALUE ZEROES.
           03  SAVE-LM-BRNO            PIC 9(04).
           03  SAVE-LM-ACCTNO          PIC 9(08).
           03  SAVE-LM-DATE            PIC 9(08).
           03  SAVE-LM-SEQ             PIC 9(03).

       01  MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "NEW: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-NEW-CHAR   PIC X(24).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).

       01  PAYOFF-QUESTION     PIC X(45) VALUE
           "IS THIS A DEALER OR INSURANCE PAYOFF? Y/N:".

       01  LEGEND          PIC X(35).
       01  LEGEND-01.
           03  FILLER      PIC X(33)  VALUE
           "F1, F7-EXIT WITH UPDATE, ENTER #:".
       01  LEGEND-02.
           03  FILLER      PIC X(8)  VALUE
           " F7-EXIT".

       01  HOLD-SYS-DATE   PIC 9(8).

       01  SUB             PIC 999    COMP  VALUE 0.
       01  ONE             PIC S9     COMP.
       01  ELE             PIC 9(2)   COMP.
       01  CHAN            PIC X(4).
       01  NEW-FLAG        PIC 9.

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

      * ORIGINAL RECORD
       COPY "LIBLP/LP01LTL.CPY" REPLACING LEADING "LTL" BY "LTL-OR".
      * CHANGED RECORD
       COPY "LIBLP/LP01LTL.CPY" REPLACING LEADING "LTL" BY "LTL-CH".

       01  HOLD-LIEN-FIELDS.
           03  HOLD-LIEN-NAME        PIC X(25).
           03  HOLD-LIEN-ADR         PIC X(25).
           03  HOLD-LIEN-CITY        PIC X(15).
           03  HOLD-LIEN-STATE       PIC X(2).
           03  HOLD-LIEN-ZIP         PIC X(10).

       01  HOLD-LIEN-TITLE-STATE     PIC XX VALUE SPACES.

       01  IO-TYPE         PIC X(3).
       01  FIRST-TIME      PIC 9     VALUE 0.

       01  FORM-NUMBER     PIC 999   VALUE ZEROS.

      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  D-ADDITIONAL-LIENS    PIC X.
           03  D-SEND-TO-CUSTOMER    PIC X.
           03  D-SEND-NAME           PIC X(25).
           03  D-SEND-ADR            PIC X(25).
           03  D-SEND-ADR2           PIC X(22).
           03  D-SEND-CITY           PIC X(15).
           03  D-SEND-STATE          PIC X(2).
           03  D-SEND-ZIP            PIC X(10).
           03  D-COMMENT1            PIC X(40).
           03  D-COMMENT2            PIC X(40).
           03  D-SEND-ASAP           PIC X.
           03  D-SENT-DATE           PIC 99/99/99.
           03  D-SENT-BY             PIC X(25).
           03  D-SEND-FMNO           PIC ZZZ.
           03  D-CARRIER-TYPE        PIC X(4).
           03  D-CARRIER-ACCTNO      PIC X(20).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(58) VALUE
           "LIENS 010 020 030 031 040 041 042 050 051 060 070 071 072 ".
           03  FILLER      PIC X(8) VALUE
           "080 081.".

       01  MAX             PIC 99   VALUE 8.

       01  SPEC-TABLE.
      *01. SEND TITLE TO CUSTOMER
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *02. SEND TITLE NAME
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      *03. SEND TITLE ADDRESS:
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      *04. SEND TITLE CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      *05. COMMENTS
           03  FILLER      PIC X(8) VALUE "E  A400 ".
      *06. SEND ASAP
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *07. TITLE SENT DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *08. CARRIER TYPE
           03  FILLER      PIC X(8) VALUE "E  A040 ".

       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 8 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.


       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/FDSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/PASSWDW.CPY".
       01  SEND-CUST-SEQ      PIC 99 VALUE 01.
       01  SEND-TITLE-SEQ     PIC 99 VALUE 02.

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TITLEW_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/TITLEW_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/TITLEW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/TITLEW_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME TITLE-PROCESSING-BUF EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 22 TO WINDOW-LINES.
           MOVE 78 TO WINDOW-COLUMNS.
           MOVE 2 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE "WI/TITLE" TO WINDOW-FORM-NAM.
           MOVE "TITLE" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY TITLE-SCREEN.

           MOVE TITLE-FORM-FIELDS TO WR-BUF.
           MOVE TITLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "N" TO TITLE-CLEAR-PAYOFF-LINE.

           MOVE TITLE-LN-OWNBR  TO LN-OWNBR.
           MOVE TITLE-LN-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " " TO BR-CHANGE-MEMO
              MOVE "N" TO BR-COLL-ID-SCAN.


           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

           PERFORM GET-GPENV.

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
           ELSE
              PERFORM FORM-RESET.

           MOVE SPACES TO IO-TYPE.
           PERFORM DISPLAY-FIELDS.

           IF D-ADDITIONAL-LIENS = "Y" OR TITLE-FROM-LONPFA NOT = "Y"
              MOVE 0 TO NEW-FLAG
           ELSE
              MOVE 1  TO NEW-FLAG.

           PERFORM ENTRY-MODULE.
      * REGACC REQUESTED THE F1-CLEAR RETURN YOU ALL THE WAY BACK TO
      * CLEAR THE PO POSTING LINE
           IF IO-TYPE = "CLR"
              IF TITLE-FROM-LONPFA = "Y"
                 GO TO END-ROUTINE
              ELSE
                 GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.


      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO IO-TYPE.
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

           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
      *    MOVE LEGEND-01 TO LEGEND.
      *    PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "Y" TO TITLE-CLEAR-PAYOFF-LINE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              IF (TITLE-FROM-LONPFA = "Y" AND
                                    (LTL-TITLE-NAME = SPACES OR
                                     LTL-TITLE-ADR = SPACES OR
                                     LTL-TITLE-CITY = SPACES OR
                                     LTL-TITLE-STATE = SPACES OR
                                     LTL-TITLE-ZIP = SPACES) )
                 MOVE "NAME AND ADDRESS MUST BE FILLED IN" TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "END" TO IO-TYPE
                 PERFORM UPDATE-LT
                 PERFORM CREATE-CHANGE-REC
                 GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.
           IF (TITLE-FROM-LONPFA = "Y")
              IF ELE = 7
                 IF NEW-FLAG = 1
                    GO TO CREATE-ROUTINE
                 ELSE
                    MOVE "NOT ALLOWED WITH THIS TRANSACTION" TO MESS
                    PERFORM SEND-MESS
                    GO TO MODIFY-ROUTINE.

      *    IF ELE = 8
      *       IF WK-TITLE-DATE = ZEROS
      *          IF NEW-FLAG = 1
      *             GO TO CREATE-ROUTINE.
      *          ELSE
      *             GO TO MODIFY-ROUTINE.

           IF ELE = 1
              MOVE SEND-CUST-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE CUSTOMER FLAG:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 IF NEW-FLAG = 1
                    GO TO CREATE-ROUTINE
                 ELSE
                    GO TO MODIFY-ROUTINE.

           IF ELE = 2 OR ELE = 3 OR ELE = 4
              MOVE SEND-TITLE-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE SEND TITLE TO:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 IF NEW-FLAG = 1
                    GO TO CREATE-ROUTINE
                 ELSE
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
       TEST-F6.
      *    IF ELE = 8
      *       IF F6-KEY
      *          PERFORM FORM-SCAN
      *          IF IO-FG NOT = ZEROS
      *             GO TO ENTER-ELE.

       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              IF TITLE-FROM-LONPFA = "Y"
                 MOVE "Y" TO TITLE-CLEAR-PAYOFF-LINE
                 GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07
                 A-08
                  DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************

       A-01.
           MOVE VDUBUF TO LTL-TITLE-SEND-FLAG.
           IF LTL-TITLE-SEND-FLAG NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.

           IF LTL-TITLE-SEND-FLAG = "Y" AND D-ADDITIONAL-LIENS = "Y"
              MOVE "SHOULD BE 'N' BECAUSE OF ADDITIONAL LIENS" TO MESS
              PERFORM SEND-MESS.

           IF LTL-TITLE-SEND-FLAG = "Y"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM OPEN-BW1-FILE
              PERFORM READ-BW1-FILE
              PERFORM CLOSE-BW1-FILE
              IF IO-FG = 0
                 MOVE "S  A250020." TO VDU
                 MOVE SPACES TO VDUBUF
                 STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   " INTO
                                                 VDUBUF
                 MOVE VDUBUF TO LTL-TITLE-NAME
                 PERFORM SCREENIO
                 MOVE "S  A250030." TO VDU
                 MOVE BW-ADR1 TO VDUBUF LTL-TITLE-ADR
                 PERFORM SCREENIO
                 MOVE "S  A220031." TO VDU
                 MOVE BW-ADR2 TO VDUBUF LTL-TITLE-ADR2
                 PERFORM SCREENIO
                 MOVE "S  A250040." TO VDU
                 MOVE BW-CITY TO VDUBUF LTL-TITLE-CITY
                 PERFORM SCREENIO
                 MOVE "S  A020041." TO VDU
                 MOVE BW-STATE TO VDUBUF LTL-TITLE-STATE
                 PERFORM SCREENIO
                 MOVE "S  A100042." TO VDU
                 MOVE BW-ZIP TO VDUBUF LTL-TITLE-ZIP
                 PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       A-02.
           MOVE VDUBUF TO LTL-TITLE-NAME.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO LTL-TITLE-ADR.
       A-031.
           MOVE "E  A220031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-031.
           MOVE VDUBUF TO LTL-TITLE-ADR2.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO LTL-TITLE-CITY.
       A-041.
           MOVE "E  A020041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-041.
           MOVE VDUBUF TO LTL-TITLE-STATE ST-CODE.

           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG = 9
             GO TO A-041.

       A-042.
           MOVE "E  A100042." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-041.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-042.
           MOVE VDUBUF TO LTL-TITLE-ZIP.

           GO TO CREATE-ROUTINE.

       A-05.
           MOVE VDUBUF TO LTL-TITLE-COMMENT1.
       A-051.
           MOVE "E  A400051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-051.
           MOVE VDUBUF TO LTL-TITLE-COMMENT2.
           GO TO CREATE-ROUTINE.

       A-06.
           MOVE VDUBUF TO LTL-SEND-ASAP.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-07.
      *    IF TITLE-FROM-LONPFA = "Y"
      *       GO TO CREATE-ROUTINE.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE LTL-TITLE-DATE.

           IF VDUNUM0 NOT = 0
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 MOVE "S  A250071." TO VDU
                 MOVE EXT-CONAME-USER-NM TO VDUBUF LTL-TITLE-SENT-BY
                 PERFORM SCREENIO.

       A-072.
           IF LTL-TITLE-DATE = 0
              MOVE 0 TO FORM-NUMBER LTL-TITLE-SEND-FMNO
              GO TO CREATE-ROUTINE.

           MOVE "E  N030072." TO VDU.
           PERFORM SCREENIO.
           IF F6-KEY
              PERFORM FORM-SCAN.
           IF VDUSTATUS = "1U"
             GO TO ENTER-ELE .
           IF NOT (VDUSTATUS = "00" OR "1D")
             GO TO A-072.

           MOVE VDUNUM0 TO FORM-NUMBER LTL-TITLE-SEND-FMNO.

           IF VDUNUM0 NOT = 0
              MOVE LTL-TITLE-SEND-FMNO TO FD-FMNO
              PERFORM OPEN-FD1-FILE
              PERFORM READ-FD1-FILE
              PERFORM CLOSE-FD1-FILE
              IF IO-FG NOT = 0
                 MOVE "INVALID FORM #" TO MESS PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-08.
           IF VDUBUF NOT = "UPS" AND NOT = "FDXP" AND
                      NOT = "EXML" AND NOT = "PRML" AND
                       NOT = "AIRB" AND NOT = SPACES
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LTL-CARRIER-TYPE.
       A-081.
           MOVE "E  A200081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
             GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
             GO TO A-081.
           MOVE VDUBUF TO LTL-CARRIER-ACCTNO.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************
       SET-RELEASE-TYPE SECTION.
      ************************************
           IF HOLD-LIEN-TITLE-STATE = SPACES
              MOVE "S  A020RELEASE-TYPE." TO VDU
              MOVE "LO" TO LTL-RELEASE-TYPE VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-SET-RELEASE-TYPE.

           MOVE HOLD-LIEN-TITLE-STATE TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG NOT = 0
              MOVE "S  A020RELEASE-TYPE." TO VDU
              MOVE "LO" TO LTL-RELEASE-TYPE VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-SET-RELEASE-TYPE.


           IF ST-TITLE-RELEASE-TYPE NOT = "A"
              MOVE "S  A020RELEASE-TYPE." TO VDU
              MOVE "LO" TO LTL-RELEASE-TYPE VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-SET-RELEASE-TYPE.

           MOVE "S  A430011-P." TO VDU.
           MOVE PAYOFF-QUESTION TO VDUBUF.
           PERFORM SCREENIO.


       UGLY-ENTER-AGAIN.
           MOVE "E  A010012." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              GO TO UGLY-ENTER-AGAIN.

           IF VDUBUF = "N"
              MOVE "S  A020RELEASE-TYPE." TO VDU
              MOVE "LO" TO LTL-RELEASE-TYPE VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A020RELEASE-TYPE." TO VDU
              MOVE "DI" TO LTL-RELEASE-TYPE VDUBUF
              PERFORM SCREENIO.

       EXIT-SET-RELEASE-TYPE.
           EXIT.

      *********************
       UPDATE-LT SECTION.
      *********************
           PERFORM OPEN-LTL1-FILE.

      * IF ORIGINAL IS NOT = TO WHAT YOU JUST BUILT
           IF LTL-OR-REC NOT = LTL-REC
              PERFORM WRITE-LTL1-FILE
              IF IO-FG NOT = 0
                 PERFORM REWRITE-LTL1-FILE
              END-IF
              IF FORM-NUMBER NOT = ZERO
                 PERFORM CREATE-FORM-REQUEST.

           PERFORM CLOSE-LTL1-FILE.

       EXIT-UPDATE-LT.
           EXIT.

      ****************************************
       CREATE-FORM-REQUEST SECTION.
      ****************************************

      *    PERFORM OPEN-SP1-FILE.
      *    MOVE LN-ORGST TO SP-ORGST.
      *    MOVE LN-SPRCLASS TO SP-SPRCLASS.
      *    MOVE LN-SUBCLASS TO SP-SUBCLASS.
      *    MOVE LN-LAWCODE TO SP-LAWCODE.
      *    PERFORM OPEN-SP1-FILE.
      *    PERFORM READ-SP1-FILE.
      *    PERFORM CLOSE-SP1-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO CREATE-FORM-REQUEST-EXIT.

           IF ( FD-OPTION-EXTRACTION ) 
               PERFORM GET-FM-PATH.

           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

           MOVE TITLE-LN-ACCTNO     TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE LTL-TITLE-SEND-FMNO TO LPWSFM-EDIT-FM-FORMNO.
           IF ( FD-OPTION-EXTRACTION ) 
              MOVE LN-OWNBR         TO LPWSFM-EDIT-FM-BRNO2.

           MOVE FM-PATH-OWNBR           TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY  TO FM-INITKEY.
           MOVE LTL-TITLE-SEND-FMNO     TO FM-FMNO.

           MOVE BW-ZIP             TO FM-ZIP.
           MOVE LN-OWNBR           TO FM-BRNO.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE HOLD-SYS-DATE      TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           PERFORM WRITE-FM1-FILE.
           IF ( IO-BAD               ) AND 
              ( FD-OPTION-EXTRACTION )
              PERFORM REWRITE-FM1-FILE.

           IF IO-FG = 9 AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           PERFORM CLOSE-FM1-FILE.

       CREATE-FORM-REQUEST-EXIT.
           EXIT.

      **************************
       GET-FM-PATH SECTION.
      **************************

           IF GP-CORP-BRNO = 0
              GO TO EXIT-GET-FM-PATH.

           MOVE GP-CORP-BRNO TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO EXIT-GET-FM-PATH.

      * USE CORPORATE BRANCH DATA PATH
           MOVE "Y"              TO FM-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE TO FM-ALL-BASE.
           MOVE BR-MACHINE       TO FM-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO FM-ALL-DATA.
           MOVE FM-ALL-PATH      TO FM-PATH.

       EXIT-GET-FM-PATH.
           EXIT.

      ************************
       FORM-SCAN SECTION.
      ************************
           MOVE 0   TO FDSCAN-KEY.
           MOVE "C" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
              MOVE "XX" TO VDUSTATUS
              GO TO EXIT-FORM-SCAN.

           MOVE "S  N030072." TO VDU.
           MOVE FDSCAN-KEY TO LTL-TITLE-SEND-FMNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "00" TO VDUSTATUS.

       EXIT-FORM-SCAN.
           EXIT.

      **************************
       CREATE-CHANGE-REC SECTION.
      **************************
           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-LM1-FILE.

           IF LTL-OR-TITLE-SEND-FLAG NOT = LTL-TITLE-SEND-FLAG
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE SEND FG"        TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-SEND-FLAG TO CH-ORG-CHAR
              MOVE LTL-TITLE-SEND-FLAG    TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-NAME NOT = LTL-TITLE-NAME
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE NAME"           TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-NAME      TO CH-ORG-CHAR
              MOVE LTL-TITLE-NAME         TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-ADR NOT = LTL-TITLE-ADR
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE ADR" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-ADR TO CH-ORG-CHAR
              MOVE LTL-TITLE-ADR TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-ADR2 NOT = LTL-TITLE-ADR2
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE ADR2" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-ADR2 TO CH-ORG-CHAR
              MOVE LTL-TITLE-ADR2 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-CITY NOT = LTL-TITLE-CITY
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE CITY" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-CITY TO CH-ORG-CHAR
              MOVE LTL-TITLE-CITY TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-STATE NOT = LTL-TITLE-STATE
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE STATE" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-STATE TO CH-ORG-CHAR
              MOVE LTL-TITLE-STATE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-ZIP NOT = LTL-TITLE-ZIP
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE ZIP" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-ZIP TO CH-ORG-CHAR
              MOVE LTL-TITLE-ZIP TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-SEND-ASAP NOT = LTL-SEND-ASAP
              PERFORM CREATE-SETUP-REC
              MOVE "SEND ASAP" TO CH-DESCRIPT
              MOVE LTL-OR-SEND-ASAP TO CH-ORG-CHAR
              MOVE LTL-SEND-ASAP TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           IF LTL-OR-TITLE-DATE NOT = LTL-TITLE-DATE
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE DATE" TO CH-DESCRIPT
              MOVE SPACES TO CH-NEW-CHAR
              MOVE LTL-OR-TITLE-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO CH-ORG-DATE
              MOVE LTL-TITLE-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO CH-NEW-DATE
              PERFORM UPDATE-CH-LM-FILE.

           IF LTL-OR-TITLE-SENT-BY NOT = LTL-TITLE-SENT-BY
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE SENT BY" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-SENT-BY TO CH-ORG-CHAR
              MOVE LTL-TITLE-SENT-BY TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-SEND-FMNO NOT = LTL-TITLE-SEND-FMNO
              PERFORM CREATE-SETUP-REC
              MOVE "SEND FORM NO"   TO CH-DESCRIPT
              MOVE SPACES TO CH-ORG-CHAR CH-NEW-CHAR
              MOVE LTL-OR-TITLE-SEND-FMNO TO CH-ORG-NUM
              MOVE LTL-TITLE-SEND-FMNO   TO CH-NEW-NUM
              PERFORM UPDATE-CH-LM-FILE.

           IF LTL-OR-TITLE-COMMENT1 NOT = LTL-TITLE-COMMENT1
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE COMMENT1" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-COMMENT1 TO CH-ORG-CHAR
              MOVE LTL-TITLE-COMMENT1 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-TITLE-COMMENT2 NOT = LTL-TITLE-COMMENT2
              PERFORM CREATE-SETUP-REC
              MOVE "TITLE COMMENT2" TO CH-DESCRIPT
              MOVE LTL-OR-TITLE-COMMENT2 TO CH-ORG-CHAR
              MOVE LTL-TITLE-COMMENT2 TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-CARRIER-TYPE NOT = LTL-CARRIER-TYPE
              PERFORM CREATE-SETUP-REC
              MOVE "CARRIER TYPE" TO CH-DESCRIPT
              MOVE LTL-OR-CARRIER-TYPE TO CH-ORG-CHAR
              MOVE LTL-CARRIER-TYPE TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.
           IF LTL-OR-CARRIER-ACCTNO NOT = LTL-CARRIER-ACCTNO
              PERFORM CREATE-SETUP-REC
              MOVE "CARRIER ACCTNO" TO CH-DESCRIPT
              MOVE LTL-OR-CARRIER-ACCTNO TO CH-ORG-CHAR
              MOVE LTL-CARRIER-ACCTNO TO CH-NEW-CHAR
              PERFORM UPDATE-CH-LM-FILE.

           PERFORM CLOSE-CH-FILE.
           GO TO EXIT-CREATE-CHANGE-REC.

       CREATE-SETUP-REC.
           PERFORM CLEAR-CH-FILE.
           MOVE EXT-CONAME-USER-LOC TO CH-BRNO.
           MOVE HOLD-SYS-DATE       TO CH-DATE.
           MOVE "T"                 TO CH-CODE.
           MOVE EXT-CONAME-USER-ID  TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS            TO CH-TIME.
           MOVE 1                   TO CH-SEQ.
           MOVE LN-ACCTNO           TO CH-ACCTNO.

       EXIT-CREATE-CHANGE-REC.
           EXIT.

      ************************************
       UPDATE-CH-LM-FILE SECTION.
      ************************************
       UPDATE-CH-AGAIN.
           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF CH-SEQ < 999
                 ADD 1 TO CH-SEQ
                 GO TO UPDATE-CH-AGAIN.

           IF BR-CHANGE-MEMO NOT = "Y"
              GO TO UPDATE-CH-LM-FILE-EXIT.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - HOLD-SYS-DATE.
           MOVE 1             TO SAVE-LM-SEQ.
       UPDATE-CH-LM-FILE-AGAIN.
           MOVE SPACES TO LM-REC.

           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE CH-USERID    TO MEMO-USERID.
           MOVE CH-DESCRIPT  TO MEMO-DESCRIPT.
           MOVE CH-NEW-CHAR  TO MEMO-NEW-CHAR.
           MOVE MEMO-AREA    TO LM-MEMO.
           MOVE "9"          TO LM-MEMO-TYPE.

           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF SAVE-LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO UPDATE-CH-LM-FILE-AGAIN.

       UPDATE-CH-LM-FILE-EXIT.
           EXIT.

      *************************
       DISPLAY-FIELDS SECTION.
      *************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE SPACES TO LTL-OR-REC LTL-CH-REC.

           MOVE "N" TO D-ADDITIONAL-LIENS.

      * TEST THE THREE "NAMES OF LIENHOLDER" FIELDS IN THE LIEN TRAILER,
      * IF ANY ARE FILLED IN, THEN ADDITIONAL-TITLES MUST BE "Y"

           PERFORM OPEN-LTL1-FILE.

           MOVE LN-OWNBR  TO LTL-BRNO.
           MOVE LN-ACCTNO TO LTL-ACCTNO.
           MOVE 1         TO LTL-SEQNO.
           PERFORM READ-LTL1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-LTL-FILE
              MOVE "N" TO LTL-SEND-ASAP
              MOVE LN-OWNBR  TO LTL-BRNO
              MOVE LN-ACCTNO TO LTL-ACCTNO
              MOVE 1         TO LTL-SEQNO
           ELSE
              IF LTL-LIEN-NAME(1) NOT = SPACES OR
                  LTL-LIEN-NAME(2) NOT = SPACES OR
                   LTL-LIEN-NAME(3) NOT = SPACES
                 MOVE "Y" TO D-ADDITIONAL-LIENS
                 IF LTL-LIEN-NAME(1) NOT = SPACES
                    MOVE 1 TO SUB
                 ELSE
                    IF LTL-LIEN-NAME(2) NOT = SPACES
                       MOVE 2 TO SUB
                    ELSE
                       MOVE 3 TO SUB.
 
            IF D-ADDITIONAL-LIENS = "Y"
               MOVE LTL-LIEN-NAME(SUB)  TO HOLD-LIEN-NAME
               MOVE LTL-LIEN-ADR(SUB)   TO HOLD-LIEN-ADR
               MOVE LTL-LIEN-CITY(SUB)  TO HOLD-LIEN-CITY
               MOVE LTL-LIEN-STATE(SUB) TO HOLD-LIEN-STATE
               MOVE LTL-LIEN-ZIP(SUB)   TO HOLD-LIEN-ZIP.


      * NEED THIS HERE TO GET IT INTO THE DEFAULT RECORD
      * AUTOMATICALLY "N" IF ADDITIONAL LIENS ARE SHOWING
           IF D-ADDITIONAL-LIENS = "Y"
              MOVE "N" TO LTL-TITLE-SEND-FLAG.

           MOVE LTL-REC TO LTL-OR-REC LTL-CH-REC.

           MOVE LTL-TITLE-SEND-FLAG TO D-SEND-TO-CUSTOMER.

           MOVE LTL-TITLE-NAME      TO D-SEND-NAME.
           MOVE LTL-TITLE-ADR       TO D-SEND-ADR.
           MOVE LTL-TITLE-ADR2      TO D-SEND-ADR2.
           MOVE LTL-TITLE-CITY      TO D-SEND-CITY.
           MOVE LTL-TITLE-STATE     TO D-SEND-STATE.
           MOVE LTL-TITLE-ZIP       TO D-SEND-ZIP.
           MOVE LTL-SEND-ASAP       TO D-SEND-ASAP.
           MOVE LTL-TITLE-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-SENT-DATE.
           MOVE LTL-TITLE-SENT-BY   TO D-SENT-BY.
           MOVE LTL-TITLE-SEND-FMNO TO D-SEND-FMNO.

           IF TITLE-FROM-LONPFA = "Y"
             IF D-ADDITIONAL-LIENS = "Y"
               MOVE HOLD-LIEN-NAME    TO D-SEND-NAME  
               MOVE HOLD-LIEN-ADR     TO D-SEND-ADR   
               MOVE SPACES            TO D-SEND-ADR2  
               MOVE HOLD-LIEN-CITY    TO D-SEND-CITY  
               MOVE HOLD-LIEN-STATE   TO D-SEND-STATE
               MOVE HOLD-LIEN-ZIP     TO D-SEND-ZIP   
               MOVE LTL-SEND-ASAP     TO D-SEND-ASAP  
               MOVE LTL-TITLE-DATE    TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY       TO D-SENT-DATE 
               MOVE LTL-TITLE-SENT-BY TO D-SENT-BY 
               MOVE LTL-TITLE-SEND-FMNO TO D-SEND-FMNO.


      * NEED TO GET LT-L-TITLE-STATE FOR SETTING OF RELEASE-TYPE
           MOVE LTL-LIEN-TITLE-STATE TO HOLD-LIEN-TITLE-STATE.

           MOVE LTL-TITLE-COMMENT1 TO D-COMMENT1.
           MOVE LTL-TITLE-COMMENT2 TO D-COMMENT2.
           MOVE LTL-CARRIER-TYPE   TO D-CARRIER-TYPE.
           MOVE LTL-CARRIER-ACCTNO TO D-CARRIER-ACCTNO.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CLOSE-LTL1-FILE.


       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLTLCL.CPY".
       COPY "LIBLP/LPCHCL.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      ************************
       VERIFY-PASSWORD SECTION.
      ************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "TITLE" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************
       FORM-RESET SECTION.
      ***************************************
           DISPLAY OMITTED ERASE SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TITLE-SCREEN.
           MOVE TITLE-FORM-FIELDS TO WR-BUF.
           MOVE TITLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      **************************************
       SETUP-LINK-INFO SECTION.
      **************************************
           MOVE TITLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE TITLE-HELP--DIR TO HELP-LINK-DIR.

      **************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TITLEW_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARAGRAPHS SECTION.
      **************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      ************************************
       IO-ROUTINE SECTION.
      ************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTL1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLTL1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ************************************
       END-ROUTINE SECTION.
      ************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TITLE-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
