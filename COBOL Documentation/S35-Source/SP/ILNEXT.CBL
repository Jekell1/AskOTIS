       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ILNEXT.
      *********************************************************************
      *
      *    CREATE ASCII FILE FOR ILLINOIS SMALL LOAN REPORTING
      *           VERITEC
      *
      *    WRITES FILE  "../../FTP/ILL/WFCILN.YYMMDD",  PIPE DELIMITED
      *    DRIVES OFF LOAN MASTER
      *    ALL CLASSES EXCEPT 50 AND 55
      *    ONLY INCLUDES LN-ORGST = "IL"
      *    SKIPS VOIDS
      *    BEG-DATE CAN NOT BE PRIOR TO 8/1/22
      *    MUST BE IN LOAN DATE RANGE (ENTDATE IF CONVERTED)
      *
      *    FILE INCLUDES INFO ON FROM 1 TO 4 BORROWERS (WE SEND 2)
      *
      *  REV:
      *  20220907 BAH SAVED OLD PROGRAM IN SV, NEW VERITEC EXTRACTION LAYOUT
      *               AND RULES. #1570
      *  20221019 BAH CHANGED ILN-NUMBER-OF-PAYS TO Z(4) #1570A
      *  20221024 BAH SKIP TRANSFERRED ACCOUNTS #1570B
      *  RMZ 20231006 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *  20240702 BAH SKIP CLASS 55 #1662 S35Q-75
      *  20250319 BAH ADD SIGNIFICANT COLLCD "SC" AND "WS" #1705 S35Q-185
      *********************************************************************
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/ILNEXT.CBL S35 11/22/23-13:02:09 >".

      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBSP/ILLPATH.CPY".
       COPY "LIBSP/SPWSILLINOIS.CPY".

       01  JUSTIFY-LEFT          PIC X        VALUE "L".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-BEG-DATE         PIC X(10).
       01  QWS-END-DATE         PIC X(10).
       01  QWS-BRANCH-RECORDS   PIC S9(13)  COMP-3 VALUE 0.
       01  QWS-BRANCH-BYPASSED  PIC S9(13)  COMP-3 VALUE 0.
       01  QWS-BRANCH-INCLUDED  PIC S9(13)  COMP-3 VALUE 0.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW      PIC X        VALUE "N".
       01  LN3-TOTAL-OPEN-SW     PIC X        VALUE "N".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99   COMP VALUE 0.
           03  LN-COUNT         PIC 999  COMP VALUE 99.
           03  LN-WK            PIC 999  COMP VALUE 0.
       01  PAGE-NUMBER          PIC 9(4) COMP VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(24) VALUE SPACES.

       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-INCLUDED       PIC S9(13)  COMP-3 VALUE 0.
       01  SUB                  PIC 99.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9999.
           03  END-BR          PIC 9999.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.

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
           03  FILLER           PIC X(12)    VALUE " ".
           03  FILLER           PIC X(42) VALUE
           "--- ILLINOIS NEW SMALL LOAN EXTRACTION ---".
       01  HEAD3.
           03  FILLER           PIC X(9)    VALUE "BRANCH NO".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(13)   VALUE "TOTAL RECORDS".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(12)   VALUE "TOTAL LN AMT".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(11)   VALUE "TOTAL FILES".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE-PATH.
               05  RANGE1       PIC X(10).
               05  RANGE1N REDEFINES RANGE1 PIC Z(9)9.
               05  RANGE1D REDEFINES RANGE1 PIC 9999/99/99.
               05  FILLER       PIC X(2).
               05  RANGE2       PIC X(10).
               05  RANGE2N REDEFINES RANGE2 PIC Z(9)9.
               05  RANGE2D REDEFINES RANGE2 PIC 9999/99/99.
               05  FILLER       PIC X(20).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ILNEXT_DEF.CPY".
       COPY "LIBSP/ILNEXT_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ILNEXT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE ILLINOIS-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE ILLINOIS-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ILNEXT" TO VDU-FORM-NAME.

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

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              LTC-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-TOTAL-LN3-FILE.
           PERFORM READ-TOTAL-LN3-FILE-NEXT. *> SSP - LNFILE
           PERFORM CLOSE-TOTAL-LN3-FILE. *> EMBEDDED - LNFILE

           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LTC1-FILE.

       END-OF-GROUP.
          
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      **************************
       GLOBAL-UPDATE SECTION.
      **************************

           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-BRNO
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LTC1-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE. *> SSP - LNFILE
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.  *> EMBEDDED - LNFILE
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-GLOBAL-UPDATE.

      *    ADD 1 TO TOTAL-RECORDS.

           IF LN-POCD-NONLOAN
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

      * SKIP TRANSFERRED ACCOUNTS, NEW BRANCH WILL REPORT THEM
           IF LN-POCD = "BT"
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-ORGST NOT = "IL"
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR > END-CLASS
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-CLASS = 50 OR 55
      *       ADD 1 TO TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-CONVERCD = "Y"
              MOVE LN-ENTDATE TO NUM-DATE
           ELSE
              MOVE LN-LOANDATE TO NUM-DATE.
           IF NUM-DATE < BEG-DATE
      *       ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF NUM-DATE > END-DATE
      *       ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      *    MOVE LN-ORGST    TO SP-ORGST.
      *    MOVE LN-SPRCLASS TO SP-SPRCLASS.
      *    MOVE LN-SUBCLASS TO SP-SUBCLASS.
      *    MOVE LN-LAWCODE  TO SP-LAWCODE.
      *    IF LPWSSP-SP1-KEY NOT = SP1-KEY
      *       MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE
      *       IF IO-FG NOT = 0
           IF SP-ORGST     = "%"
                 GO TO IO-ERROR.

      *    ADD 1 TO TOTAL-INCLUDED.

           PERFORM CREATE-WORK.

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE.
           EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE ILNEXT-QUEUE-POS TO Q-POS.
           MOVE ILNEXT-COPIES-POS TO C-POS.
           MOVE ILNEXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE. 

           PERFORM OPEN-PR-FILE.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           MOVE 0 TO TOTAL-RECORDS
                     TOTAL-BYPASSED                   
                     TOTAL-INCLUDED.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD-DD TO ILN-PATH-DD.
           MOVE DATE-YYYYMMDD-MM TO ILN-PATH-MM.
           MOVE DATE-YYYYMMDD-YY TO ILN-PATH-YY.
           CALL "C$TOLOWER" USING ILN-FTP-PATH, VALUE 9.

           PERFORM LOAD-ILN-FILE.
           MOVE ILN-PATH TO ACCESS-BUF ILLINOIS-PATH.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO CONTINUE-INITIALIZE.

           IF EXT-ROUTE-BUF NOT = "00"
              GO TO CONTINUE-INITIALIZE.

       OVERWRITE-QUESTION.
           MOVE "ILN FILE EXISTS FOR THIS DATE. CONTINUE? (Y/N)"
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
           GO TO INITIALIZE-ENDIT.

       CONTINUE-INITIALIZE.

           PERFORM OPEN-ILLINOIS-FILE-OUTPUT.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.
      * BEGIN DATE CAN NOT BE PRIOR TO 8/1/22
           IF EXT-EOBUF-DATE1 < 20220801
              MOVE 20220801 TO BEG-DATE
              MOVE 20220831 TO END-DATE.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.

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
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO CL-01.
      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO DATE-01.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 1             TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 05            TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      * ONLY GET LOANS 8/1/22 AND BEYOND
           IF BEG-DATE < 20220801
              MOVE "S  8080DATE1." TO VDU
              MOVE 20220801          TO BEG-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO
              MOVE BEG-DATE        TO VDUNUM0.

      *-----------------------------------------------------------
       DATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO DATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              IF END-DATE < BEG-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO DATE-01.

           GO TO EXIT-PARM.

       EXIT-PARM.
           EXIT.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020CL1." TO VDU
           MOVE BEG-CLASS    TO VDUNUM0
           PERFORM SCREENIO
           MOVE "SNNN020CL2." TO VDU
           MOVE END-CLASS    TO VDUNUM0
           PERFORM SCREENIO.

      ****************************
       CREATE-WORK SECTION.
      ****************************

      * LEAVE FIELDS BLANK IF NO ENTRY
 
           MOVE SPACES TO ILLINOIS-FIELDS.

           MOVE "NEW"                TO ILN-ACTION.

           MOVE BR-VERITEC-LICENSE   TO ILN-LICENSE-ID.

           MOVE LN-OWNBR             TO ILN-LENDER-BRNO.
           MOVE LN-ACCTNO            TO ILN-ACCTNO.

           MOVE 0 TO ILN-CUST-COUNT.
           PERFORM CREATE-WORK-BORROWER VARYING SUB FROM 1 BY 1
                                        UNTIL SUB > 2.

           PERFORM LOAN-CALCULATIONS.

      * INSTALLMENT (IL), TITLE LOAN (TL)
      *             COLLCD AP,AU,WA,WB = "TL" ELSE "IL"
           IF LN-COLLCD = "AP" OR "AU" OR "WA" OR "WB"
              MOVE "TL"              TO ILN-LOAN-TYPE
           ELSE
              MOVE "IL"              TO ILN-LOAN-TYPE.

           MOVE LN-LOANDATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY        TO ILN-AGREEMENT-DATE.

      * WANT ORIGINAL MATURITY (NO DEFERMENTS ADDED IN)
           MOVE "Y" TO MDTE-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY        TO ILN-MATURITY-DATE.

      * PRINCIPAL/AMOUNT FINANCED
           MOVE FINANCED-AMOUNT      TO ILN-PRINCIPAL-AMOUNT.

      * TOTAL OF ALL PAYMENTS
           MOVE TOTAL-REPAY          TO ILN-TOTAL-AMOUNT.

      * PRECOMPUTE (PM) OR INTEREST BEARING (IB)
           IF LN-LOANTYPE = "I"
              MOVE "IB"              TO ILN-INTEREST-TYPE
           ELSE
              MOVE "PC"              TO ILN-INTEREST-TYPE.

      * NUMBER OF SCHEDULED PAYMENTS
           MOVE LN-ORGTERM           TO ILN-NUMBER-OF-PAYS.

      * SCHEDULED PAYMENT AMOUNT
           MOVE LN-REGPYAMT          TO ILN-SCHEDULED-PAYMENT.

      * SECURITY

      * DEFAULT TO "OT"  (GN, PL, RE)

           MOVE "OT"                   TO ILN-SECURITY.
           MOVE SPACES                 TO ILN-SECURITY-DESC.

           IF LN-COLLCD = "PP" OR "PO" OR "SC"
              MOVE "PP"                TO ILN-SECURITY
           ELSE
           IF LN-COLLCD = "WG"
              MOVE "WA"                TO ILN-SECURITY
           ELSE
           IF LN-COLLCD = "AU"
              MOVE "VH"                TO ILN-SECURITY
           ELSE
           IF LN-COLLCD = "GN" OR "PL" OR "RE"
              MOVE "OTHER"                       TO ILN-SECURITY-DESC
           ELSE
           IF LN-COLLCD = "00"
              MOVE "CONVERSION ACCOUNT"          TO ILN-SECURITY-DESC
           ELSE
           IF LN-COLLCD = "AP"
              MOVE "AUTO AND PERSONAL PROPERTY"  TO ILN-SECURITY-DESC
           ELSE
      * UNSECURED, PURCHASED ACCT NOT SECURED
           IF LN-COLLCD = "UL" OR "US"
              MOVE "NONE"                        TO ILN-SECURITY-DESC
           ELSE
           IF LN-COLLCD = "WA"
              MOVE "WAGE ASSIGNMENT AND AUTO"    TO ILN-SECURITY-DESC
           ELSE
           IF LN-COLLCD = "WB"
              MOVE "WAGE ASSIGNMENT AUTO AND PP" TO ILN-SECURITY-DESC
           ELSE
           IF LN-COLLCD = "WP" OR "WS"
              MOVE "WAGE ASSIGNMENT AND PERSONAL PROPERTY"
                                                 TO ILN-SECURITY-DESC.

           MOVE LN-APRATE        TO ILN-APRATE
                                    ILN-PLPA-APRATE.

      * DOES THIS LOAN HAVE A PRIOR PAYOFF
           IF LN-SRCD = "PB" OR "RN" OR "RO"
              MOVE "Y"           TO ILN-PAYOFF
           ELSE
              MOVE "N"           TO ILN-PAYOFF.

      *    MOVE LN-OWNBR  TO LTC-BRNO.
      *    MOVE LN-ACCTNO TO LTC-ACCTNO.
      *    MOVE 1         TO LTC-SEQNO.
      *    PERFORM READ-LTC1-FILE.
      *    IF IO-FG = 0
           IF LTC-DESC(1) NOT = "%"
              MOVE LTC-VIN(1)    TO ILN-VIN.

      * THIS MUST ALSO INCLUDE ALL THE UPDATE FIELDS, BLANK

      * THE STRING ARE SPLIT UP INTO SECTIONS JUST TO MAKE IT SAFER.

           MOVE SPACES TO ILN-HEADER-INFO.
           STRING ILN-ACTION, "|",
                  ILN-LICENSE-ID, "|",
                  ILN-LENDER-ID, "|",
                  ILN-CUST-COUNT,"|"
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
           CALL "C$JUSTIFY" USING ILN-PRINCIPAL-AMOUNT, JUSTIFY-LEFT.
           CALL "C$JUSTIFY" USING ILN-TOTAL-AMOUNT, JUSTIFY-LEFT.
           CALL "C$JUSTIFY" USING ILN-SCHEDULED-PAYMENT, JUSTIFY-LEFT.
           CALL "C$JUSTIFY" USING ILN-APRATE, JUSTIFY-LEFT.
           CALL "C$JUSTIFY" USING ILN-PLPA-APRATE, JUSTIFY-LEFT.
           CALL "C$JUSTIFY" USING ILN-NUMBER-OF-PAYS, JUSTIFY-LEFT.
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

           MOVE "N" TO FIRST-TIME.

       EXIT-CREATE-WORK.
           EXIT.

      ***************************************
       CREATE-WORK-BORROWER SECTION.
      ***************************************

           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.  *> EMBEDDED - BWFILE
           IF IO-FG NOT = 0
              IF SUB = 1
                 GO TO IO-ERROR
              ELSE
                 GO TO EXIT-CREATE-WORK-BORROWER.

           ADD 1                     TO ILN-CUST-COUNT.

           MOVE BW-SSNO              TO ILN-SSNO(SUB).
           MOVE "SSN"                TO ILN-ID-TYPE(SUB).
           MOVE BW-FNAME             TO ILN-FNAME(SUB).
           MOVE BW-LNAME             TO ILN-LNAME(SUB).
           MOVE BW-ADR1              TO ILN-ADR1(SUB).
           MOVE BW-ADR2              TO ILN-ADR2(SUB).
           MOVE BW-CITY              TO ILN-CITY(SUB).
           MOVE BW-STATE             TO ILN-STATE(SUB).
           MOVE BW-ZIP               TO ILN-ZIP(SUB).

       EXIT-CREATE-WORK-BORROWER.
           EXIT.

      ***************************
       PRINT-RANGE-LINES SECTION.
      ***************************

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

           MOVE "BEG - END CLASS" TO RANGE-SELECTION.
           MOVE BEG-CLASS TO RANGE1N.
           MOVE END-CLASS TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG - END LOAN DATE" TO RANGE-SELECTION.
           MOVE BEG-DATE TO RANGE1D.
           MOVE END-DATE TO RANGE2D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS SUBMITTED" TO RANGE-SELECTION.
           MOVE TOTAL-INCLUDED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OUTPUT FILE PATH" TO RANGE-SELECTION.
           MOVE ILN-PATH TO RANGE-PATH.
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
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO ILNEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ILNEXT-SYSDATE.
           DISPLAY ILNEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ILNEXT-FORM-FIELDS TO WR-BUF.
           MOVE ILNEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE ILNEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE ILNEXT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ILNEXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************
       MISC-ROUTINES SECTION.
      ***********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      **********************
       IO-ROUTINE SECTION.
      **********************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER
                               AS LN3_CONNECT 
                   USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

           IF LN3-TOTAL-OPEN-SW = "N" 
              MOVE "Y"         TO LN3-TOTAL-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER
                               AS LN3_CONNECT_TOTAL 
                   USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE. *> SSP - LN,LTC,SPA
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

           MOVE BEG-DATE                TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE. 
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-BEG-DATE.

           MOVE END-DATE                TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-END-DATE.
    

      *  STORED PROCEDURE:
      *
      *   SELECT LNFILE.LN_OWNBR,
      *          LNFILE.LN_ACCTNO,
      *          LNFILE.LN_CLASS,
      *          LNFILE.LN_SUBCLASS,
      *          LNFILE.LN_LAWCODE,
      *          LNFILE.LN_SSNO1,
      *          LNFILE.LN_MAKERCD1, 
      *          LNFILE.LN_SSNO2,
      *          LNFILE.LN_MAKERCD2,
      *          LNFILE.LN_SSNO3,
      *          LNFILE.LN_MAKERCD3,
      *          LNFILE.LN_SSNO4,
      *          LNFILE.LN_MAKERCD4,
      *          LNFILE.LN_ORGST,
      *          LNFILE.LN_SPRCLASS,
      *          LNFILE.LN_CONVERCD, 
      *          LNFILE.LN_SRCD, 
      *          LNFILE.LN_COLLCD, 
      *          LNFILE.LN_POCD, 
      *          LNFILE.LN_LOANTYPE, 
      *          CAST(LNFILE.LN_ENTDATE        AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)), 
      *          LNFILE.LN_INTCHG,
      *          LNFILE.LN_SERCHG,
      *          LNFILE.LN_EXTCHG,
      *          LNFILE.LN_MAINTFEE,
      *          LNFILE.LN_ORGTERM,
      *          LNFILE.LN_1STPYAMT,
      *          LNFILE.LN_NOREGPY,
      *          LNFILE.LN_REGPYAMT,
      *          LNFILE.LN_LASTPYAMT,
      *          LNFILE.LN_FEECODE_1, 
      *          LNFILE.LN_FEECODE_2, 
      *          LNFILE.LN_FEECODE_3, 
      *          LNFILE.LN_FEECODE_4, 
      *          LNFILE.LN_FEECODE_5, 
      *          LNFILE.LN_FEECODE_6, 
      *          LNFILE.LN_FEECODE_7, 
      *          LNFILE.LN_FEECODE_8, 
      *          LNFILE.LN_FEECODE_9, 
      *          LNFILE.LN_FEECODE_10, 
      *          LNFILE.LN_FEECODE_11, 
      *          LNFILE.LN_FEECODE_12, 
      *          LNFILE.LN_FEECODE_13, 
      *          LNFILE.LN_FEECODE_14, 
      *          LNFILE.LN_FEECODE_15, 
      *          LNFILE.LN_FEECODE_16, 
      *          LNFILE.LN_FEECODE_17, 
      *          LNFILE.LN_FEECODE_18, 
      *          LNFILE.LN_FEECODE_19, 
      *          LNFILE.LN_FEECODE_20, 
      *          LNFILE.LN_FEEAMT_1,
      *          LNFILE.LN_FEEAMT_2,
      *          LNFILE.LN_FEEAMT_3,
      *          LNFILE.LN_FEEAMT_4,
      *          LNFILE.LN_FEEAMT_5,
      *          LNFILE.LN_FEEAMT_6,
      *          LNFILE.LN_FEEAMT_7,
      *          LNFILE.LN_FEEAMT_8,
      *          LNFILE.LN_FEEAMT_9,
      *          LNFILE.LN_FEEAMT_10,
      *          LNFILE.LN_FEEAMT_11,
      *          LNFILE.LN_FEEAMT_12,
      *          LNFILE.LN_FEEAMT_13,
      *          LNFILE.LN_FEEAMT_14,
      *          LNFILE.LN_FEEAMT_15,
      *          LNFILE.LN_FEEAMT_16,
      *          LNFILE.LN_FEEAMT_17,
      *          LNFILE.LN_FEEAMT_18,
      *          LNFILE.LN_FEEAMT_19,
      *          LNFILE.LN_FEEAMT_20,
      *          LNFILE.LN_FEEOURS_1, 
      *          LNFILE.LN_FEEOURS_2, 
      *          LNFILE.LN_FEEOURS_3, 
      *          LNFILE.LN_FEEOURS_4, 
      *          LNFILE.LN_FEEOURS_5, 
      *          LNFILE.LN_FEEOURS_6, 
      *          LNFILE.LN_FEEOURS_7, 
      *          LNFILE.LN_FEEOURS_8, 
      *          LNFILE.LN_FEEOURS_9, 
      *          LNFILE.LN_FEEOURS_10, 
      *          LNFILE.LN_FEEOURS_11, 
      *          LNFILE.LN_FEEOURS_12, 
      *          LNFILE.LN_FEEOURS_13, 
      *          LNFILE.LN_FEEOURS_14, 
      *          LNFILE.LN_FEEOURS_15, 
      *          LNFILE.LN_FEEOURS_16, 
      *          LNFILE.LN_FEEOURS_17, 
      *          LNFILE.LN_FEEOURS_18, 
      *          LNFILE.LN_FEEOURS_19, 
      *          LNFILE.LN_FEEOURS_20, 
      *          LNFILE.LN_FEEFINCHG_1, 
      *          LNFILE.LN_FEEFINCHG_2, 
      *          LNFILE.LN_FEEFINCHG_3, 
      *          LNFILE.LN_FEEFINCHG_4, 
      *          LNFILE.LN_FEEFINCHG_5, 
      *          LNFILE.LN_FEEFINCHG_6, 
      *          LNFILE.LN_FEEFINCHG_7, 
      *          LNFILE.LN_FEEFINCHG_8, 
      *          LNFILE.LN_FEEFINCHG_9, 
      *          LNFILE.LN_FEEFINCHG_10, 
      *          LNFILE.LN_FEEFINCHG_11, 
      *          LNFILE.LN_FEEFINCHG_12, 
      *          LNFILE.LN_FEEFINCHG_13, 
      *          LNFILE.LN_FEEFINCHG_14, 
      *          LNFILE.LN_FEEFINCHG_15, 
      *          LNFILE.LN_FEEFINCHG_16, 
      *          LNFILE.LN_FEEFINCHG_17, 
      *          LNFILE.LN_FEEFINCHG_18, 
      *          LNFILE.LN_FEEFINCHG_19, 
      *          LNFILE.LN_FEEFINCHG_20, 
      *          LNFILE.LN_INSOURS_1, 
      *          LNFILE.LN_INSOURS_2, 
      *          LNFILE.LN_INSOURS_3, 
      *          LNFILE.LN_INSOURS_4, 
      *          LNFILE.LN_INSOURS_5, 
      *          LNFILE.LN_INSOURS_6, 
      *          LNFILE.LN_INSOURS_7, 
      *          LNFILE.LN_INSOURS_8, 
      *          LNFILE.LN_INSOURS_9, 
      *          LNFILE.LN_INSOURS_10, 
      *          LNFILE.LN_INSPREM_1,
      *          LNFILE.LN_INSPREM_2,
      *          LNFILE.LN_INSPREM_3,
      *          LNFILE.LN_INSPREM_4,
      *          LNFILE.LN_INSPREM_5,
      *          LNFILE.LN_INSPREM_6,
      *          LNFILE.LN_INSPREM_7,
      *          LNFILE.LN_INSPREM_8,
      *          LNFILE.LN_INSPREM_9,
      *          LNFILE.LN_INSPREM_10,
      *          LNFILE.LN_INSFINCHG_1, 
      *          LNFILE.LN_INSFINCHG_2, 
      *          LNFILE.LN_INSFINCHG_3, 
      *          LNFILE.LN_INSFINCHG_4, 
      *          LNFILE.LN_INSFINCHG_5, 
      *          LNFILE.LN_INSFINCHG_6, 
      *          LNFILE.LN_INSFINCHG_7, 
      *          LNFILE.LN_INSFINCHG_8, 
      *          LNFILE.LN_INSFINCHG_9, 
      *          LNFILE.LN_INSFINCHG_10, 
      *          LNFILE.LN_ANTICERN_1,
      *          LNFILE.LN_ANTICERN_2,
      *          LNFILE.LN_ANTICERN_3,
      *          LNFILE.LN_ANTICERN_4,
      *          LNFILE.LN_ANTICERN_5,
      *          LNFILE.LN_ANTICERN_6,
      *          LNFILE.LN_ANTICERN_7,
      *          LNFILE.LN_ANTICERN_8,
      *          LNFILE.LN_ANTICERN_9,
      *          LNFILE.LN_ANTICERN_10,
      *          LNFILE.LN_TOTNODEF,
      *          LNFILE.LN_CREDITLIM,
      *          LNFILE.LN_SUM_ORGTERM,
      *          LNFILE.LN_UNITPER_CD,
      *          LNFILE.LN_UNITPER_FREQ,
      *          ISNULL(LTCFILE.LTC_BRNO, 0),
      *          ISNULL(LTCFILE.LTC_ACCTNO, 0),
      *          ISNULL(LTCFILE.LTC_SEQNO, 0), 
      *          ISNULL(LTCFILE.LTC_DESC_1,'%'),
      *          ISNULL(LTCFILE.LTC_MAKE_1,' '),
      *          ISNULL(LTCFILE.LTC_MODEL_1,' '),
      *          ISNULL(LTCFILE.LTC_YEAR_1,' '),
      *          ISNULL(LTCFILE.LTC_STYLE_1,' '),
      *          ISNULL(LTCFILE.LTC_CLASS_1,' '),
      *          ISNULL(LTCFILE.LTC_VIN_1,' '),
      *          ISNULL(LTCFILE.LTC_TITLE_NO_1,' '),
      *          ISNULL(LTCFILE.LTC_MILEAGE_1, 0),
      *          ISNULL(LTCFILE.LTC_VALUE_1, 0),
      *          ISNULL(LTCFILE.LTC_SELLPR_1, 0),
      *          ISNULL(LTCFILE.LTC_MISC_AMT_1, 0),
      *          ISNULL(LTCFILE.LTC_SENT_FLAG_1,' '),
      *          ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
      *                               AS VARCHAR(10)),'1900-01-01'),
      *          ISNULL(LTCFILE.LTC_DESC_2,' '),
      *          ISNULL(LTCFILE.LTC_MAKE_2,' '),
      *          ISNULL(LTCFILE.LTC_MODEL_2,' '),
      *          ISNULL(LTCFILE.LTC_YEAR_2,' '),
      *          ISNULL(LTCFILE.LTC_STYLE_2,' '),
      *          ISNULL(LTCFILE.LTC_CLASS_2,' '),
      *          ISNULL(LTCFILE.LTC_VIN_2,' '),
      *          ISNULL(LTCFILE.LTC_TITLE_NO_2,' '),
      *          ISNULL(LTCFILE.LTC_MILEAGE_2, 0),
      *          ISNULL(LTCFILE.LTC_VALUE_2, 0),
      *          ISNULL(LTCFILE.LTC_SELLPR_2, 0),
      *          ISNULL(LTCFILE.LTC_MISC_AMT_2, 0),
      *          ISNULL(LTCFILE.LTC_SENT_FLAG_2,' '),
      *          ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
      *                               AS VARCHAR(10)),'1900-01-01'),
      *          ISNULL(LTCFILE.LTC_DESC_3,' '),
      *          ISNULL(LTCFILE.LTC_MAKE_3,' '),
      *          ISNULL(LTCFILE.LTC_MODEL_3,' '),
      *          ISNULL(LTCFILE.LTC_YEAR_3,' '),
      *          ISNULL(LTCFILE.LTC_STYLE_3,' '),
      *          ISNULL(LTCFILE.LTC_CLASS_3,' '),
      *          ISNULL(LTCFILE.LTC_VIN_3,' '),
      *          ISNULL(LTCFILE.LTC_TITLE_NO_3,' '),
      *          ISNULL(LTCFILE.LTC_MILEAGE_3, 0),
      *          ISNULL(LTCFILE.LTC_VALUE_3, 0),
      *          ISNULL(LTCFILE.LTC_SELLPR_3, 0),
      *          ISNULL(LTCFILE.LTC_MISC_AMT_3, 0),
      *          ISNULL(LTCFILE.LTC_SENT_FLAG_3,' '),
      *          ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
      *                               AS VARCHAR(10)),'1900-01-01'),
      *          ISNULL(SPAFILE.SPA_ORGST,'%'),
      *          ISNULL(SPAFILE.SPA_SPRCLASS, 0),
      *          ISNULL(SPAFILE.SPA_SUBCLASS, 0),
      *          ISNULL(SPAFILE.SPA_LAWCODE, 0),
      *          ISNULL(SPAFILE.SPA_MFFRMLA, ' '),
      *          ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,' '),
      *          ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,' ')
      * 
      *     FROM DBO.LNFILE  LNFILE
      * 
      *     LEFT OUTER JOIN 
      *          DBO.LTCFILE LTCFILE
      *       ON (  LTCFILE.LTC_BRNO        = LNFILE.LN_OWNBR	
      *      AND    LTCFILE.LTC_ACCTNO      = LNFILE.LN_ACCTNO
      *      AND    LTCFILE.LTC_SEQNO       = 1 )
      * 
      *     LEFT OUTER JOIN
      *          DBO.SPAFILE SPAFILE
      *       ON (  SPAFILE.SPA_ORGST        = LNFILE.LN_ORGST
      *      AND    SPAFILE.SPA_SPRCLASS     = LNFILE.LN_SPRCLASS
      *      AND    SPAFILE.SPA_SUBCLASS     = LNFILE.LN_SUBCLASS
      *      AND    SPAFILE.SPA_LAWCODE      = LNFILE.LN_LAWCODE )
      * 
      *    WHERE (  LNFILE.LN_OWNBR         = @QLN3_WBEG_OWNBR
      *      AND    LNFILE.LN_CLASS   BETWEEN @QLN3_WBEG_CLASS
      *                                   AND @QLN3_WEND_CLASS  )
      * 
      *      AND (  LNFILE.LN_POCD      NOT IN 
      *                           ('**', '*I', '*R', '*U', 'BT'))
      *      AND (  LNFILE.LN_ORGST          = 'IL' )
      *      AND (  LNFILE.LN_CLASS     NOT IN (50, 55) )
      *      AND (( LNFILE.LN_CONVERCD       = 'Y'
      *      AND    LNFILE.LN_ENTDATE  BETWEEN @QWS_BEG_DATE
      *                                    AND @QWS_END_DATE )
      *       OR  ( LNFILE.LN_CONVERCD      != 'Y'
      *      AND    LNFILE.LN_LOANDATE BETWEEN @QWS_BEG_DATE
      *                                    AND @QWS_END_DATE ))
      * 
      *    ORDER BY LNFILE.LN_OWNBR,
      *             LNFILE.LN_CLASS,
      *             LNFILE.LN_ACCTNO; 
             

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_ILNEXT_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR, 
                :QLN3-WBEG-CLASS, 
                :QLN3-WEND-CLASS,
                :QWS-BEG-DATE,
                :QWS-END-DATE)
           END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS
                          QLN3-WBEG-ACCTNO
                          QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT. *> EMBEDDED - LN,LTC,SPA
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
           INITIALIZE QLTC-REC.
           INITIALIZE QSPA-REC.
    
           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

              EXEC SQL 
                  FETCH LN3_CURSOR 
                    INTO
                     :QLN-OWNBR, 
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1, 
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-CONVERCD, 
                     :QLN-SRCD, 
                     :QLN-COLLCD, 
                     :QLN-POCD, 
                     :QLN-LOANTYPE, 
                     :QLN-ENTDATE,          
                     :QLN-LOANDATE,         
                     :QLN-ORIG-1STPYDATE,   
                     :QLN-1STPYDATE,
                     :QLN-APRATE,        
                     :QLN-INTCHG,
                     :QLN-SERCHG,
                     :QLN-EXTCHG,
                     :QLN-MAINTFEE,
                     :QLN-ORGTERM,
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-FEECODE-1, 
                     :QLN-FEECODE-2, 
                     :QLN-FEECODE-3, 
                     :QLN-FEECODE-4, 
                     :QLN-FEECODE-5, 
                     :QLN-FEECODE-6, 
                     :QLN-FEECODE-7, 
                     :QLN-FEECODE-8, 
                     :QLN-FEECODE-9, 
                     :QLN-FEECODE-10, 
                     :QLN-FEECODE-11, 
                     :QLN-FEECODE-12, 
                     :QLN-FEECODE-13, 
                     :QLN-FEECODE-14, 
                     :QLN-FEECODE-15, 
                     :QLN-FEECODE-16, 
                     :QLN-FEECODE-17, 
                     :QLN-FEECODE-18, 
                     :QLN-FEECODE-19, 
                     :QLN-FEECODE-20, 
                     :QLN-FEEAMT-1,
                     :QLN-FEEAMT-2,
                     :QLN-FEEAMT-3,
                     :QLN-FEEAMT-4,
                     :QLN-FEEAMT-5,
                     :QLN-FEEAMT-6,
                     :QLN-FEEAMT-7,
                     :QLN-FEEAMT-8,
                     :QLN-FEEAMT-9,
                     :QLN-FEEAMT-10,
                     :QLN-FEEAMT-11,
                     :QLN-FEEAMT-12,
                     :QLN-FEEAMT-13,
                     :QLN-FEEAMT-14,
                     :QLN-FEEAMT-15,
                     :QLN-FEEAMT-16,
                     :QLN-FEEAMT-17,
                     :QLN-FEEAMT-18,
                     :QLN-FEEAMT-19,
                     :QLN-FEEAMT-20,
                     :QLN-FEEOURS-1, 
                     :QLN-FEEOURS-2, 
                     :QLN-FEEOURS-3, 
                     :QLN-FEEOURS-4, 
                     :QLN-FEEOURS-5, 
                     :QLN-FEEOURS-6, 
                     :QLN-FEEOURS-7, 
                     :QLN-FEEOURS-8, 
                     :QLN-FEEOURS-9, 
                     :QLN-FEEOURS-10, 
                     :QLN-FEEOURS-11, 
                     :QLN-FEEOURS-12, 
                     :QLN-FEEOURS-13, 
                     :QLN-FEEOURS-14, 
                     :QLN-FEEOURS-15, 
                     :QLN-FEEOURS-16, 
                     :QLN-FEEOURS-17, 
                     :QLN-FEEOURS-18, 
                     :QLN-FEEOURS-19, 
                     :QLN-FEEOURS-20, 
                     :QLN-FEEFINCHG-1, 
                     :QLN-FEEFINCHG-2, 
                     :QLN-FEEFINCHG-3, 
                     :QLN-FEEFINCHG-4, 
                     :QLN-FEEFINCHG-5, 
                     :QLN-FEEFINCHG-6, 
                     :QLN-FEEFINCHG-7, 
                     :QLN-FEEFINCHG-8, 
                     :QLN-FEEFINCHG-9, 
                     :QLN-FEEFINCHG-10, 
                     :QLN-FEEFINCHG-11, 
                     :QLN-FEEFINCHG-12, 
                     :QLN-FEEFINCHG-13, 
                     :QLN-FEEFINCHG-14, 
                     :QLN-FEEFINCHG-15, 
                     :QLN-FEEFINCHG-16, 
                     :QLN-FEEFINCHG-17, 
                     :QLN-FEEFINCHG-18, 
                     :QLN-FEEFINCHG-19, 
                     :QLN-FEEFINCHG-20, 
                     :QLN-INSOURS-1, 
                     :QLN-INSOURS-2, 
                     :QLN-INSOURS-3, 
                     :QLN-INSOURS-4, 
                     :QLN-INSOURS-5, 
                     :QLN-INSOURS-6, 
                     :QLN-INSOURS-7, 
                     :QLN-INSOURS-8, 
                     :QLN-INSOURS-9, 
                     :QLN-INSOURS-10, 
                     :QLN-INSPREM-1,
                     :QLN-INSPREM-2,
                     :QLN-INSPREM-3,
                     :QLN-INSPREM-4,
                     :QLN-INSPREM-5,
                     :QLN-INSPREM-6,
                     :QLN-INSPREM-7,
                     :QLN-INSPREM-8,
                     :QLN-INSPREM-9,
                     :QLN-INSPREM-10,
                     :QLN-INSFINCHG-1, 
                     :QLN-INSFINCHG-2, 
                     :QLN-INSFINCHG-3, 
                     :QLN-INSFINCHG-4, 
                     :QLN-INSFINCHG-5, 
                     :QLN-INSFINCHG-6, 
                     :QLN-INSFINCHG-7, 
                     :QLN-INSFINCHG-8, 
                     :QLN-INSFINCHG-9, 
                     :QLN-INSFINCHG-10, 
                     :QLN-ANTICERN-1,
                     :QLN-ANTICERN-2,
                     :QLN-ANTICERN-3,
                     :QLN-ANTICERN-4,
                     :QLN-ANTICERN-5,
                     :QLN-ANTICERN-6,
                     :QLN-ANTICERN-7,
                     :QLN-ANTICERN-8,
                     :QLN-ANTICERN-9,
                     :QLN-ANTICERN-10,
                     :QLN-TOTNODEF,
                     :QLN-CREDITLIM,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ,
                     :QLTC-BRNO,  
                     :QLTC-ACCTNO,  
                     :QLTC-SEQNO,   
                     :QLTC-DESC-1, 
                     :QLTC-MAKE-1, 
                     :QLTC-MODEL-1, 
                     :QLTC-YEAR-1, 
                     :QLTC-STYLE-1, 
                     :QLTC-CLASS-1, 
                     :QLTC-VIN-1, 
                     :QLTC-TITLE-NO-1, 
                     :QLTC-MILEAGE-1,  
                     :QLTC-VALUE-1,  
                     :QLTC-SELLPR-1,  
                     :QLTC-MISC-AMT-1, 
                     :QLTC-SENT-FLAG-1, 
                     :QLTC-VIN-CHG-DATE-1,
                     :QLTC-DESC-2, 
                     :QLTC-MAKE-2,  
                     :QLTC-MODEL-2, 
                     :QLTC-YEAR-2, 
                     :QLTC-STYLE-2,
                     :QLTC-CLASS-2,
                     :QLTC-VIN-2,
                     :QLTC-TITLE-NO-2,
                     :QLTC-MILEAGE-2, 
                     :QLTC-VALUE-2, 
                     :QLTC-SELLPR-2, 
                     :QLTC-MISC-AMT-2, 
                     :QLTC-SENT-FLAG-2,
                     :QLTC-VIN-CHG-DATE-2,
                     :QLTC-DESC-3,
                     :QLTC-MAKE-3,
                     :QLTC-MODEL-3,
                     :QLTC-YEAR-3, 
                     :QLTC-STYLE-3, 
                     :QLTC-CLASS-3, 
                     :QLTC-VIN-3, 
                     :QLTC-TITLE-NO-3, 
                     :QLTC-MILEAGE-3,  
                     :QLTC-VALUE-3,  
                     :QLTC-SELLPR-3,  
                     :QLTC-MISC-AMT-3,  
                     :QLTC-SENT-FLAG-3, 
                     :QLTC-VIN-CHG-DATE-3,
                     :QSPA-ORGST,
                     :QSPA-SPRCLASS, 
                     :QSPA-SUBCLASS, 
                     :QSPA-LAWCODE, 
                     :QSPA-MFFRMLA, 
                     :QSPA-CAL-CHGABLE-1,
                     :QSPA-CAL-CHGABLE-2 
              END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
               GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
               PERFORM GET-LN-FIELDS
               PERFORM GET-LTC-FIELDS
               PERFORM GET-SPA-FIELDS.
 
          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.
  
      *-----------------------------------------------------------------
       START-TOTAL-LN3-FILE. *> SSP - LNFILE TOTALS
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           MOVE 0              TO QWS-BRANCH-RECORDS  
                                  QWS-BRANCH-BYPASSED
                                  QWS-BRANCH-INCLUDED. 

      *  STORED PROCEDURE:
      *
      *  SELECT 
      *  COUNT(*)          AS BRANCH_RECORDS, 
      *  SUM(CASE WHEN  (  LNFILE.LN_POCD          IN 
      *                    ('**', '*I', '*R', '*U', 'BT'))
      *              OR (  LNFILE.LN_ORGST         != 'IL' )
      *              OR (  LNFILE.LN_CLASS         IN (50, 55) )
      *              OR (( LNFILE.LN_CONVERCD       = 'Y'
      *             AND    LNFILE.LN_ENTDATE        < @QWS_BEG_DATE
      *              OR    LNFILE.LN_ENTDATE        > @QWS_END_DATE )
      *              OR  ( LNFILE.LN_CONVERCD      != 'Y'   
      *             AND    LNFILE.LN_LOANDATE       < @QWS_BEG_DATE
      *              OR    LNFILE.LN_LOANDATE       > @QWS_END_DATE ))
      *           THEN 1
      *           ELSE 0
      *       END)           AS BRANCH_BYPASSED,
      *  SUM(CASE WHEN  (  LNFILE.LN_POCD      NOT IN 
      *                    ('**', '*I', '*R', '*U', 'BT'))
      *             AND (  LNFILE.LN_ORGST          = 'IL' )
      *             AND (  LNFILE.LN_CLASS     NOT IN (50, 55) )
      *             AND (( LNFILE.LN_CONVERCD       = 'Y'
      *             AND    LNFILE.LN_ENTDATE  BETWEEN @QWS_BEG_DATE
      *                                           AND @QWS_END_DATE )
      *              OR  ( LNFILE.LN_CONVERCD      != 'Y'   
      *             AND    LNFILE.LN_LOANDATE BETWEEN @QWS_BEG_DATE
      *                                           AND @QWS_END_DATE ))
      *           THEN 1
      *           ELSE 0
      *       END)           AS BRANCH_INCLUDED
      * 
      *     FROM DBO.LNFILE  LNFILE
      *    WHERE ( LNFILE.LN_OWNBR         = @QLN3_WBEG_OWNBR
      *      AND   LNFILE.LN_CLASS   BETWEEN @QLN3_WBEG_CLASS
      *                                  AND @QLN3_WEND_CLASS ))
             

           EXEC SQL SET CONNECTION LN3_CONNECT_TOTAL END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR_TOTAL CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_ILNEXT_LNFILE_TOTAL_SELECT 
              ( :QLN3-WBEG-OWNBR, 
                :QLN3-WBEG-CLASS, 
                :QLN3-WEND-CLASS,
                :QWS-BEG-DATE,
                :QWS-END-DATE)
           END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR_TOTAL
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-TOTAL-LN3-FILE-NEXT. *> EMBEDDED - LNFILE TOTALS
           PERFORM READ-IT.
 
           EXEC SQL SET CONNECTION LN3_CONNECT_TOTAL END-EXEC.

              EXEC SQL 
                FETCH LN3_CURSOR_TOTAL
                  INTO
                   :QWS-BRANCH-RECORDS,  
                   :QWS-BRANCH-BYPASSED,
                   :QWS-BRANCH-INCLUDED 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TOTAL-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              ADD QWS-BRANCH-RECORDS  TO TOTAL-RECORDS         
              ADD QWS-BRANCH-BYPASSED TO TOTAL-BYPASSED        
              ADD QWS-BRANCH-INCLUDED TO TOTAL-INCLUDED.  

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TOTAL-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT_TOTAL END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR_TOTAL
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-CONN-OPEN-SW.
  
           IF LN3-TOTAL-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT_TOTAL END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT_TOTAL END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-TOTAL-OPEN-SW.
 

       COPY "LIBLP/LPLTC1RN.CPY".

      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-BW1-FILE. *> EMBEDDED - BWFILE
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME, 
                   BWFILE.BW_FNAME, 
                   BWFILE.BW_ADR1, 
                   BWFILE.BW_ADR2, 
                   BWFILE.BW_CITY, 
                   BWFILE.BW_STATE, 
                   BWFILE.BW_ZIP
              INTO            
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME, 
                   :QBW-FNAME, 
                   :QBW-ADR1, 
                   :QBW-ADR2, 
                   :QBW-CITY, 
                   :QBW-STATE, 
                   :QBW-ZIP
              FROM DBO.BWFILE
             WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
               AND BWFILE.BW_SSNO  = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.


       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBSP/SPILLINOISIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **********************
       END-PROGRAM SECTION.
      **********************
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
              DESTROY ILNEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
