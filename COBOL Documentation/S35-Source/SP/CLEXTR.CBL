       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLEXTR.
       DATE-WRITTEN.    08/28/2017
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  CANNED LOAN DATA WAREHOUSE EXTRACTION
      *
      * CREATES AN OUTPUT EXTRACT FILE  /USR/EXTRACT/CANNED_LOAN
      *
      * RSZ 935 INCLUDING CR,LF
      *
      * REV:
      *  BAH 2017.0828 WORLD PR#1272
      *  BAH 2020.0811 CHANGE XCL-FINAMT TO BE DISCLOSED-FINAMT, #1455
      *  RMZ 2023.1004 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *  BAH 2024.0522 ADDED XCL-4-SERCHG,XCL-8-SERCHG,XCL-CLOSING-FEE,
      *                XCL-CLASS, XCL-SUBCLASS, XCL-TN-AL-ALT-RATE-ACQ-FEE
      *                                                      #1658 S35Q-34
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXCL.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXCL.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CLEXTR.CBL S35 05/28/24-08:38:21 >".

       01  XCL-PATH.
           03  XCL-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XCL-FILENAME       PIC X(11) VALUE "CANNED_LOAN".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBLP/LP01PD.CPY".
       
       01  XCL-KEY       PIC 9(9)     VALUE 0.

       01  WS-AMT          PIC S9(7)V99 COMP-3 VALUE 0.

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.
       01  CL-CONN-OPEN-SW      PIC X        VALUE "N".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       
           03  GROUP-ID               PIC X(4).
           03  BEG-CAN-NO             PIC X(2).
           03  END-CAN-NO             PIC X(2).

       01  VERSION-CONTROL            PIC X VALUE "A".

       COPY "LIBGB/PASSWDW.CPY".

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-TITLE          PIC X(27)    VALUE
           "CANNED LOAN FILE EXTRACTION".

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  FILLER           PIC X(14).

       01  RECORD-COUNT           PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(111).

       COPY "LIBLP/LPAMTSW.CPY".
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
       COPY "LIBSP/CLEXTR_DEF.CPY".
       COPY "LIBSP/CLEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CLEXTR_SCN.CPY".

       
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XCL-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-CL1-CONNECTION.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XCL-FILE.
       
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
           MOVE "CLEXTR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-CL1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           IF GROUP-ID = "****"
              MOVE SPACES TO CL-GROUP-ID
           ELSE
              MOVE GROUP-ID   TO CL-GROUP-ID.
           MOVE BEG-CAN-NO TO CL-CAN-NO.

           PERFORM START-CL1-FILE. *> SSP - CL,SPA

       NEXT-CL.
           PERFORM READ-CL1-FILE-NEXT. *> EMBEDDED - CL,SPA
           IF IO-FG = 9
              GO TO END-ROUTINE.

           MOVE CL-PD-REC TO PD-REC.

           IF GROUP-ID NOT = "****"
              IF CL-GROUP-ID NOT = GROUP-ID
                 GO TO NEXT-CL.
              
           IF CL-CAN-NO < BEG-CAN-NO OR CL-CAN-NO > END-CAN-NO
              GO TO NEXT-CL.

      *    MOVE PD-ORGST    TO SP-ORGST.
      *    MOVE PD-SPRCLASS TO SP-SPRCLASS.
      *    MOVE PD-SUBCLASS TO SP-SUBCLASS.
      *    MOVE PD-LAWCODE  TO SP-LAWCODE.
      *    PERFORM READ-SP1-FILE.
      *    IF IO-FG NOT = 0
           IF SP-ORGST = "%"  
              GO TO NEXT-CL.

           PERFORM LOAN-CALCULATIONS.

           PERFORM FILL-EXTRACT-RECORD.
 
           GO TO NEXT-CL.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ****************************************************
       FILL-EXTRACT-RECORD SECTION.
      ****************************************************
           MOVE SPACES TO XCL-REC.

           MOVE CL-GROUP-ID TO XCL-GROUP-ID.
           MOVE CL-CAN-NO   TO XCL-CAN-NO.
           MOVE CL-DESC     TO XCL-DESC.
           MOVE CL-GIFTNET  TO XCL-GIFTNET.

      * CL-PD-REC IS NOW IN PD-REC
           MOVE PD-APRATE       TO XCL-APRATE.
           MOVE PD-SRCD         TO XCL-SRCD.
           MOVE PD-COLLCD       TO XCL-COLLCD.
           MOVE PD-ORGTERM      TO XCL-ORGTERM.
           MOVE PD-LNAMT        TO XCL-LNAMT.
           MOVE PD-INTCHG       TO XCL-INTCHG.
           MOVE PD-FINCHG       TO XCL-FINCHG.
           MOVE PD-1STPYDATE    TO XCL-1STPYDATE.
           MOVE PD-INSURED      TO XCL-INSURED.
           MOVE PD-INSTYPES     TO XCL-INSTYPES.
           MOVE PD-INSELIGIBLE  TO XCL-INSELIGIBLE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              MOVE PD-INSCOMP(SUB)          TO XCL-INSCOMP(SUB)
              MOVE PD-INSOURS(SUB)          TO XCL-INSOURS(SUB)
              MOVE 0                        TO XCL-INSEFF(SUB)
              MOVE 0                        TO XCL-INSEXP(SUB)
              MOVE PD-INSPREM(SUB)          TO XCL-INSPREM(SUB)
              MOVE PD-INSCOVR(SUB)          TO XCL-INSCOVR(SUB)
              MOVE PD-INSCOMM(SUB)          TO XCL-INSCOMM(SUB)
              MOVE PD-INSANTICERN(SUB)      TO XCL-INSANTICERN(SUB)
              INSPECT XCL-INSANTICERN(SUB) REPLACING LEADING "+" BY " "
              MOVE PD-INSANTICADJ(SUB)      TO XCL-INSANTICADJ(SUB)
              INSPECT XCL-INSANTICADJ(SUB) REPLACING LEADING "+" BY " "
              MOVE PD-INSUFERN(SUB)         TO XCL-INSUFERN(SUB)
              INSPECT XCL-INSUFERN(SUB) REPLACING LEADING "+" BY " "
              MOVE PD-INSACCERN(SUB)        TO XCL-INSACCERN(SUB)
              INSPECT XCL-INSACCERN(SUB) REPLACING LEADING "+" BY " "
              MOVE PD-INSEARN-ORDER(SUB)    TO XCL-INSEARN-ORDER(SUB)
              MOVE PD-INSFINCHG(SUB)        TO XCL-INSFINCHG(SUB)
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE PD-FEECODE(SUB)          TO XCL-FEECODE(SUB)
              MOVE PD-FEEAMT(SUB)           TO XCL-FEEAMT(SUB)
              MOVE PD-FEEOURS(SUB)          TO XCL-FEEOURS(SUB)
              MOVE PD-FEEFINCHG(SUB)        TO XCL-FEEFINCHG(SUB)
           END-PERFORM.

      * #1455 MATCH THE TILA
      *    MOVE PD-FINAMT          TO XCL-FINAMT.
           MOVE DISCLOSED-FINAMT   TO XCL-FINAMT.
           INSPECT XCL-FINAMT REPLACING LEADING "+" BY " ".
           MOVE PD-PROCEEDS        TO XCL-PROCEEDS.
           INSPECT XCL-PROCEEDS REPLACING LEADING "+" BY " ".
           MOVE PD-EFFRATE         TO XCL-EFFRATE.
           MOVE PD-PURCD           TO XCL-PURCD.
           MOVE PD-SERCHG          TO XCL-SERCHG.
           MOVE PD-EXTCHG          TO XCL-EXTCHG.
           MOVE PD-MAINTFEE        TO XCL-MAINTFEE.
           MOVE PD-REGPYAMT        TO XCL-REGPYAMT.
      * 8% SERCHG  4% SERCHG
      * 4% MAY BE 0 AT TIMES AS 8% IS ALLOWED ON THE FIRST $600 OF
      *            (TOTAL REPAY - MAINTFEE) SO THERE WOULD BE NO 4%
           IF PD-STATE-WORKER NOT = 0
              MOVE PD-STATE-WORKER TO XCL-8-SERCHG
              COMPUTE WS-AMT = PD-SERCHG - PD-STATE-WORKER - PD-POINTS
              MOVE WS-AMT          TO XCL-4-SERCHG
           ELSE
              MOVE 0               TO XCL-8-SERCHG
                                      XCL-4-SERCHG.
           MOVE PD-POINTS          TO XCL-CLOSING-FEE.
           MOVE PD-CLASS           TO XCL-CLASS.
           MOVE PD-SUBCLASS        TO XCL-SUBCLASS.

      * TN AL ALTERNATE RATE ACQUISITION FEE
           COMPUTE WS-AMT = PD-SERCHG - PD-POINTS.
           MOVE WS-AMT             TO XCL-TN-AL-ALT-RATE-ACQ-FEE.

           MOVE X"0D" TO XCL-CR.
           MOVE X"0A" TO XCL-LF.

           ADD 1 TO XCL-KEY.
           PERFORM WRITE-XCL-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CLEXTR-QUEUE-POS TO Q-POS.
           MOVE CLEXTR-COPIES-POS TO C-POS.
           MOVE CLEXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XCL-DIRECTORY, VALUE 13.
           MOVE XCL-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE " " TO EXT-EOBUF-ERRCD.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS.............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
      *    PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-XCL-FILE-OUTPUT.
           PERFORM CLOSE-XCL-FILE.
           PERFORM OPEN-XCL-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       
       ENTER-PARM.

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO NO-02.

       ENTER-GROUP.
           MOVE "E  A040GROUP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-GROUP.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-GROUP.
           MOVE VDUBUF TO GROUP-ID.
           
           IF GROUP-ID = "****"
              GO TO ENTER-GROUP.
           GO TO NO-01.
       ALL-GROUP.
           MOVE "S  A040GROUP." TO VDU.
           MOVE "****" TO GROUP-ID VDUBUF.
           PERFORM SCREENIO.

       
       NO-01.
           MOVE "E  A020BEG-NO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-GROUP.
           IF VDUSTATUS = "16" GO TO ALL-NO.
           IF VDUSTATUS = "1U" GO TO ENTER-GROUP.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO NO-01.
           MOVE VDUBUF TO BEG-CAN-NO.
       NO-02.
           MOVE "E  A020END-NO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-GROUP.
           IF VDUSTATUS = "1U" GO TO NO-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO NO-02.
           MOVE VDUBUF TO END-CAN-NO.

           GO TO EXIT-PARM.

       ALL-NO.
           MOVE "S  A020BEG-NO." TO VDU.
           MOVE SPACES TO BEG-CAN-NO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020END-NO." TO VDU.
           MOVE ALL "Z" TO END-CAN-NO.
           CALL "C$TOLOWER" USING END-CAN-NO, VALUE 2.
           MOVE END-CAN-NO TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           MOVE "S  A040GROUP." TO VDU.
           MOVE GROUP-ID TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-NO." TO VDU.
           MOVE BEG-CAN-NO TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020END-NO." TO VDU.
           MOVE END-CAN-NO TO VDUBUF.
           PERFORM SCREENIO.


      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
      *
      *     P R I N T - R A N G E - L I N E S
      *
      ******************************************************************
       PRINT-RANGE-LINES SECTION.
           MOVE 4 TO LN-FEED.
       
           MOVE "GROUP              :" TO RANGE-LINE.
           MOVE GROUP-ID               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGINNING CAN LOAN :" TO RANGE-LINE.
           MOVE BEG-CAN-NO             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    CAN LOAN :" TO RANGE-LINE.
           MOVE END-CAN-NO             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

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
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

           
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPDAMTS.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CLEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLEXTR-SYSDATE.
           DISPLAY CLEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CLEXTR-FORM-FIELDS TO WR-BUF.
           MOVE CLEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CLEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CLEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
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
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXCLI.CPY".

      *COPY "LIBLP/LPCL1RN.CPY".
      * COPYMEMBER: LIBLP/LPCL1RN
       LOAD-CL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CL-ALL-MACHINE
              MOVE CL-ALL-PATH             TO CL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CL-ALL-BASE
              MOVE FILPATH-MACHINE         TO CL-ALL-MACHINE
              MOVE CL-ALL-PATH             TO CL-PATH.


      *-----------------------------------------------------------------
       OPEN-CL1-FILE.
           PERFORM LOAD-CL1-FILE.
           PERFORM OPEN-IT.
           MOVE CL-PATH-SQL TO E-FILE.

            IF CL-CONN-OPEN-SW = "N"
              MOVE "Y" TO CL-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CL_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-CL1-FILE. *> SSP - CL,SPA
           PERFORM START-IT.
           MOVE CL-PATH-SQL TO E-FILE.
           MOVE CL1-KEY TO E-KEYX.

           IF ( CL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CL1-FILE.

           IF ( QCL1-WEND-CAN-NO = SPACES )
              MOVE CL-GROUP-ID          TO QCL1-WBEG-GROUP
              MOVE ALL "Z"              TO QCL1-WEND-GROUP
              MOVE CL-CAN-NO            TO QCL1-WBEG-CAN-NO
              MOVE ALL "Z"              TO QCL1-WEND-CAN-NO.

      *  STORED PROCEDURE:
      *
      *    SELECT CLFILE.CL_GROUP_ID, 
      *           CLFILE.CL_CAN_NO, 
      *           CLFILE.CL_DESC, 
      *           CLFILE.CL_GIFTNET, 
      *           CLFILE.CL_PD_ID, 
      *           CLFILE.CL_PD_BRANCH,
      *           CLFILE.CL_PD_SOC_SEC,
      *           CLFILE.CL_PD_OWNBR,
      *           CLFILE.CL_PD_ACCTNO,
      *           CLFILE.CL_PD_CLASS,
      *           CLFILE.CL_PD_SUBCLASS,
      *           CLFILE.CL_PD_LAWCODE,
      *           CLFILE.CL_PD_SSNO1,
      *           CLFILE.CL_PD_MAKERCD1,
      *           CLFILE.CL_PD_SSNO2,
      *           CLFILE.CL_PD_MAKERCD2,
      *           CLFILE.CL_PD_SSNO3,
      *           CLFILE.CL_PD_MAKERCD3,
      *           CLFILE.CL_PD_SSNO4,
      *           CLFILE.CL_PD_MAKERCD4,
      *           CLFILE.CL_PD_ORGST,
      *           CLFILE.CL_PD_ORGBR,
      *           CLFILE.CL_PD_OWNST,
      *           CLFILE.CL_PD_SPRCLASS,
      *           CLFILE.CL_PD_CONVERCD, 
      *           CLFILE.CL_PD_MIDSTREAM, 
      *           CLFILE.CL_PD_PURCD, 
      *           CLFILE.CL_PD_SRCD, 
      *           CLFILE.CL_PD_COLLCD, 
      *           CLFILE.CL_PD_COLLID, 
      *           CLFILE.CL_PD_POCD, 
      *           CLFILE.CL_PD_PLCD, 
      *           CLFILE.CL_PD_PLTYPE, 
      *           CLFILE.CL_PD_CONFIDENTCD, 
      *           CLFILE.CL_PD_ALLOTCD,
      *           CLFILE.CL_PD_RECASTCD, 
      *           CLFILE.CL_PD_SKIP_TRW, 
      *           CLFILE.CL_PD_STATUSFG, 
      *           CLFILE.CL_PD_SMALLOAN, 
      *           CLFILE.CL_PD_SALEFIN,  
      *           CLFILE.CL_PD_SERVACCT, 
      *           CLFILE.CL_PD_LOANTYPE, 
      *           CLFILE.CL_PD_ADDON_FG, 
      *           CLFILE.CL_PD_PAY_SCHLD_FG, 
      *           CLFILE.CL_PD_LC_CODE,
      *           CLFILE.CL_PD_CAN_LOAN_NO, 
      *           CLFILE.CL_PD_STATFLAGS, 
      *           CAST(CLFILE.CL_PD_ENTDATE
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_LOANDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_PURDATE				   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INTDATE				   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_CASHDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_FILEDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_RESCISSION_DATE		   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_PART_RECOURSE_DATE	   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_ORIG_1STPYDATE		   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_1STPYDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_JDDATE				   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_PLDATE				   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_PL_TRDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_ACCELDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_REDUDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_POFFDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_PO_TRDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_LEGALDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_BNKRPTDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_BNKRPT_DISMISS_DATE	   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_BNKRPT_DISCHARGE_DATE  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_BNKRPT_REAFFIRM_DATE   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_DEBTAIDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_REPODATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_REDEEMDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_SOLDDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INS_CONFIRM_DATE	   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_PRIOR_RN_FOR10_DATE	   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_LAST_RN_FOR10_DATE	   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_TRW_DOFD_DATE		   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_LAST_PYPA_DATE		   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_BT_TRDATE			   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_METRO2_LAST_PMT_DATE   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_FOLLOWUP_DATE		   
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_LAST_ACCRUAL_DATE	   
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_APRATE,
      *           CLFILE.CL_PD_EFFRATE,
      *           CLFILE.CL_PD_SMPRATE,
      *           CLFILE.CL_PD_YLDRATE,
      *           CLFILE.CL_PD_OVRDRATE,
      *           CLFILE.CL_PD_ENDORATE,
      *           CLFILE.CL_PD_JDRATE,
      *           CLFILE.CL_PD_ACCRATE,
      *           CLFILE.CL_PD_REDURATE,
      *           CLFILE.CL_PD_CONRATE,
      *           CLFILE.CL_PD_SERVPER,
      *           CLFILE.CL_PD_SELLPR,
      *           CLFILE.CL_PD_DOWNPY,
      *           CLFILE.CL_PD_TRADEIN,
      *           CLFILE.CL_PD_LNAMT,
      *           CLFILE.CL_PD_FINCHG,
      *           CLFILE.CL_PD_INTCHG,
      *           CLFILE.CL_PD_SERCHG,
      *           CLFILE.CL_PD_EXTCHG,
      *           CLFILE.CL_PD_MAINTFEE,
      *           CLFILE.CL_PD_POINTS,
      *           CLFILE.CL_PD_PROCEEDS,
      *           CLFILE.CL_PD_ORGTERM,
      *           CLFILE.CL_PD_1STPYAMT,
      *           CLFILE.CL_PD_NOREGPY,
      *           CLFILE.CL_PD_REGPYAMT,
      *           CLFILE.CL_PD_LASTPYAMT,
      *           CLFILE.CL_PD_PROCASH,
      *           CLFILE.CL_PD_TILLNO,
      *           CLFILE.CL_PD_STATE_FLAG,
      *           CLFILE.CL_PD_STATE_WORKER,
      *           CLFILE.CL_PD_STATE_WORKER2,
      *           CLFILE.CL_PD_FEECODE_1, 
      *           CLFILE.CL_PD_FEECODE_2, 
      *           CLFILE.CL_PD_FEECODE_3, 
      *           CLFILE.CL_PD_FEECODE_4, 
      *           CLFILE.CL_PD_FEECODE_5, 
      *           CLFILE.CL_PD_FEECODE_6, 
      *           CLFILE.CL_PD_FEECODE_7, 
      *           CLFILE.CL_PD_FEECODE_8, 
      *           CLFILE.CL_PD_FEECODE_9, 
      *           CLFILE.CL_PD_FEECODE_10, 
      *           CLFILE.CL_PD_FEECODE_11, 
      *           CLFILE.CL_PD_FEECODE_12, 
      *           CLFILE.CL_PD_FEECODE_13, 
      *           CLFILE.CL_PD_FEECODE_14, 
      *           CLFILE.CL_PD_FEECODE_15, 
      *           CLFILE.CL_PD_FEECODE_16, 
      *           CLFILE.CL_PD_FEECODE_17, 
      *           CLFILE.CL_PD_FEECODE_18, 
      *           CLFILE.CL_PD_FEECODE_19, 
      *           CLFILE.CL_PD_FEECODE_20, 
      *           CLFILE.CL_PD_FEEAMT_1,
      *           CLFILE.CL_PD_FEEAMT_2,
      *           CLFILE.CL_PD_FEEAMT_3,
      *           CLFILE.CL_PD_FEEAMT_4,
      *           CLFILE.CL_PD_FEEAMT_5,
      *           CLFILE.CL_PD_FEEAMT_6,
      *           CLFILE.CL_PD_FEEAMT_7,
      *           CLFILE.CL_PD_FEEAMT_8,
      *           CLFILE.CL_PD_FEEAMT_9,
      *           CLFILE.CL_PD_FEEAMT_10,
      *           CLFILE.CL_PD_FEEAMT_11,
      *           CLFILE.CL_PD_FEEAMT_12,
      *           CLFILE.CL_PD_FEEAMT_13,
      *           CLFILE.CL_PD_FEEAMT_14,
      *           CLFILE.CL_PD_FEEAMT_15,
      *           CLFILE.CL_PD_FEEAMT_16,
      *           CLFILE.CL_PD_FEEAMT_17,
      *           CLFILE.CL_PD_FEEAMT_18,
      *           CLFILE.CL_PD_FEEAMT_19,
      *           CLFILE.CL_PD_FEEAMT_20,
      *           CLFILE.CL_PD_FEEOURS_1, 
      *           CLFILE.CL_PD_FEEOURS_2, 
      *           CLFILE.CL_PD_FEEOURS_3, 
      *           CLFILE.CL_PD_FEEOURS_4, 
      *           CLFILE.CL_PD_FEEOURS_5, 
      *           CLFILE.CL_PD_FEEOURS_6, 
      *           CLFILE.CL_PD_FEEOURS_7, 
      *           CLFILE.CL_PD_FEEOURS_8, 
      *           CLFILE.CL_PD_FEEOURS_9, 
      *           CLFILE.CL_PD_FEEOURS_10, 
      *           CLFILE.CL_PD_FEEOURS_11, 
      *           CLFILE.CL_PD_FEEOURS_12, 
      *           CLFILE.CL_PD_FEEOURS_13, 
      *           CLFILE.CL_PD_FEEOURS_14, 
      *           CLFILE.CL_PD_FEEOURS_15, 
      *           CLFILE.CL_PD_FEEOURS_16, 
      *           CLFILE.CL_PD_FEEOURS_17, 
      *           CLFILE.CL_PD_FEEOURS_18, 
      *           CLFILE.CL_PD_FEEOURS_19, 
      *           CLFILE.CL_PD_FEEOURS_20, 
      *           CLFILE.CL_PD_FEEFINCHG_1, 
      *           CLFILE.CL_PD_FEEFINCHG_2, 
      *           CLFILE.CL_PD_FEEFINCHG_3, 
      *           CLFILE.CL_PD_FEEFINCHG_4, 
      *           CLFILE.CL_PD_FEEFINCHG_5, 
      *           CLFILE.CL_PD_FEEFINCHG_6, 
      *           CLFILE.CL_PD_FEEFINCHG_7, 
      *           CLFILE.CL_PD_FEEFINCHG_8, 
      *           CLFILE.CL_PD_FEEFINCHG_9, 
      *           CLFILE.CL_PD_FEEFINCHG_10, 
      *           CLFILE.CL_PD_FEEFINCHG_11, 
      *           CLFILE.CL_PD_FEEFINCHG_12, 
      *           CLFILE.CL_PD_FEEFINCHG_13, 
      *           CLFILE.CL_PD_FEEFINCHG_14, 
      *           CLFILE.CL_PD_FEEFINCHG_15, 
      *           CLFILE.CL_PD_FEEFINCHG_16, 
      *           CLFILE.CL_PD_FEEFINCHG_17, 
      *           CLFILE.CL_PD_FEEFINCHG_18, 
      *           CLFILE.CL_PD_FEEFINCHG_19, 
      *           CLFILE.CL_PD_FEEFINCHG_20, 
      *           CLFILE.CL_PD_INSURED,
      *           CLFILE.CL_PD_INSTYPES,
      *           CLFILE.CL_PD_INSELIGIBLE,
      *           CLFILE.CL_PD_INSCOMP_1,
      *           CLFILE.CL_PD_INSCOMP_2,
      *           CLFILE.CL_PD_INSCOMP_3,
      *           CLFILE.CL_PD_INSCOMP_4,
      *           CLFILE.CL_PD_INSCOMP_5,
      *           CLFILE.CL_PD_INSCOMP_6,
      *           CLFILE.CL_PD_INSCOMP_7,
      *           CLFILE.CL_PD_INSCOMP_8,
      *           CLFILE.CL_PD_INSCOMP_9,
      *           CLFILE.CL_PD_INSCOMP_10,
      *           CLFILE.CL_PD_INSOURS_1, 
      *           CLFILE.CL_PD_INSOURS_2, 
      *           CLFILE.CL_PD_INSOURS_3, 
      *           CLFILE.CL_PD_INSOURS_4, 
      *           CLFILE.CL_PD_INSOURS_5, 
      *           CLFILE.CL_PD_INSOURS_6, 
      *           CLFILE.CL_PD_INSOURS_7, 
      *           CLFILE.CL_PD_INSOURS_8, 
      *           CLFILE.CL_PD_INSOURS_9, 
      *           CLFILE.CL_PD_INSOURS_10, 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_1
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_2		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_3		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_4		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_5		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_6		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_7		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_8		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_9		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEFF_DATE_10		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_1		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_2		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_3		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_4		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_5		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_6		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_7		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_8		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_9		  
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INSEXP_DATE_10		  
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_INSPREM_1,
      *           CLFILE.CL_PD_INSPREM_2,
      *           CLFILE.CL_PD_INSPREM_3,
      *           CLFILE.CL_PD_INSPREM_4,
      *           CLFILE.CL_PD_INSPREM_5,
      *           CLFILE.CL_PD_INSPREM_6,
      *           CLFILE.CL_PD_INSPREM_7,
      *           CLFILE.CL_PD_INSPREM_8,
      *           CLFILE.CL_PD_INSPREM_9,
      *           CLFILE.CL_PD_INSPREM_10,
      *           CLFILE.CL_PD_INSCOVR_1,
      *           CLFILE.CL_PD_INSCOVR_2,
      *           CLFILE.CL_PD_INSCOVR_3,
      *           CLFILE.CL_PD_INSCOVR_4,
      *           CLFILE.CL_PD_INSCOVR_5,
      *           CLFILE.CL_PD_INSCOVR_6,
      *           CLFILE.CL_PD_INSCOVR_7,
      *           CLFILE.CL_PD_INSCOVR_8,
      *           CLFILE.CL_PD_INSCOVR_9,
      *           CLFILE.CL_PD_INSCOVR_10,
      *           CLFILE.CL_PD_INSCOMM_1,
      *           CLFILE.CL_PD_INSCOMM_2,
      *           CLFILE.CL_PD_INSCOMM_3,
      *           CLFILE.CL_PD_INSCOMM_4,
      *           CLFILE.CL_PD_INSCOMM_5,
      *           CLFILE.CL_PD_INSCOMM_6,
      *           CLFILE.CL_PD_INSCOMM_7,
      *           CLFILE.CL_PD_INSCOMM_8,
      *           CLFILE.CL_PD_INSCOMM_9,
      *           CLFILE.CL_PD_INSCOMM_10,
      *           CLFILE.CL_PD_INSANTICERN_1,
      *           CLFILE.CL_PD_INSANTICERN_2,
      *           CLFILE.CL_PD_INSANTICERN_3,
      *           CLFILE.CL_PD_INSANTICERN_4,
      *           CLFILE.CL_PD_INSANTICERN_5,
      *           CLFILE.CL_PD_INSANTICERN_6,
      *           CLFILE.CL_PD_INSANTICERN_7,
      *           CLFILE.CL_PD_INSANTICERN_8,
      *           CLFILE.CL_PD_INSANTICERN_9,
      *           CLFILE.CL_PD_INSANTICERN_10,
      *           CLFILE.CL_PD_INSANTICADJ_1,
      *           CLFILE.CL_PD_INSANTICADJ_2,
      *           CLFILE.CL_PD_INSANTICADJ_3,
      *           CLFILE.CL_PD_INSANTICADJ_4,
      *           CLFILE.CL_PD_INSANTICADJ_5,
      *           CLFILE.CL_PD_INSANTICADJ_6,
      *           CLFILE.CL_PD_INSANTICADJ_7,
      *           CLFILE.CL_PD_INSANTICADJ_8,
      *           CLFILE.CL_PD_INSANTICADJ_9,
      *           CLFILE.CL_PD_INSANTICADJ_10,
      *           CLFILE.CL_PD_INSUFERN_1,
      *           CLFILE.CL_PD_INSUFERN_2,
      *           CLFILE.CL_PD_INSUFERN_3,
      *           CLFILE.CL_PD_INSUFERN_4,
      *           CLFILE.CL_PD_INSUFERN_5,
      *           CLFILE.CL_PD_INSUFERN_6,
      *           CLFILE.CL_PD_INSUFERN_7,
      *           CLFILE.CL_PD_INSUFERN_8,
      *           CLFILE.CL_PD_INSUFERN_9,
      *           CLFILE.CL_PD_INSUFERN_10,
      *           CLFILE.CL_PD_INSACCERN_1,
      *           CLFILE.CL_PD_INSACCERN_2,
      *           CLFILE.CL_PD_INSACCERN_3,
      *           CLFILE.CL_PD_INSACCERN_4,
      *           CLFILE.CL_PD_INSACCERN_5,
      *           CLFILE.CL_PD_INSACCERN_6,
      *           CLFILE.CL_PD_INSACCERN_7,
      *           CLFILE.CL_PD_INSACCERN_8,
      *           CLFILE.CL_PD_INSACCERN_9,
      *           CLFILE.CL_PD_INSACCERN_10,
      *           CLFILE.CL_PD_INSEARN_ORDER_1,
      *           CLFILE.CL_PD_INSEARN_ORDER_2,
      *           CLFILE.CL_PD_INSEARN_ORDER_3,
      *           CLFILE.CL_PD_INSEARN_ORDER_4,
      *           CLFILE.CL_PD_INSEARN_ORDER_5,
      *           CLFILE.CL_PD_INSEARN_ORDER_6,
      *           CLFILE.CL_PD_INSEARN_ORDER_7,
      *           CLFILE.CL_PD_INSEARN_ORDER_8,
      *           CLFILE.CL_PD_INSEARN_ORDER_9,
      *           CLFILE.CL_PD_INSEARN_ORDER_10,
      *           CLFILE.CL_PD_INSFINCHG_1, 
      *           CLFILE.CL_PD_INSFINCHG_2, 
      *           CLFILE.CL_PD_INSFINCHG_3, 
      *           CLFILE.CL_PD_INSFINCHG_4, 
      *           CLFILE.CL_PD_INSFINCHG_5, 
      *           CLFILE.CL_PD_INSFINCHG_6, 
      *           CLFILE.CL_PD_INSFINCHG_7, 
      *           CLFILE.CL_PD_INSFINCHG_8, 
      *           CLFILE.CL_PD_INSFINCHG_9, 
      *           CLFILE.CL_PD_INSFINCHG_10, 
      *           CLFILE.CL_PD_PY_YYYYMM_RECDEL,
      *           CLFILE.CL_PD_MTD_PAYS_RECDEL,
      *           CLFILE.CL_PD_GARNISH_EXTR, 
      *           CLFILE.CL_PD_SHIP_SAME_FG, 
      *           CLFILE.CL_PD_AD_D_INS_COVR,
      *           CLFILE.CL_PD_TOT_PARTIAL_CHGOFF,
      *           CLFILE.CL_PD_LPO, 
      *           CLFILE.CL_PD_REP_CD, 
      *           CLFILE.CL_PD_EMP_ID,
      *           CLFILE.CL_PD_PROCESS_CD, 
      *           CLFILE.CL_PD_WCBC_CREDITLIM,
      *           CLFILE.CL_PD_FOLLOWUP_CD, 
      *           CLFILE.CL_PD_MIP_FG, 
      *           CLFILE.CL_PD_MIP_MTD_CLAHPAID,
      *           CAST(CLFILE.CL_PD_MIP_CLPDTH_DATE
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_MIP_TOT_CLPAID,					  
      *           CAST(CLFILE.CL_PD_MIP_AHPDTH_DATE		  
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_MIP_TOT_AHPAID,
      *           CLFILE.CL_PD_IL_GROSS_INCOME,
      *           CLFILE.CL_PD_IL_GROSS_INCOME2,
      *           CLFILE.CL_PD_RESCISSION, 
      *           CLFILE.CL_PD_INTERIM_TRW, 
      *           CLFILE.CL_PD_DLNO,
      *           CLFILE.CL_PD_DLPOOL,
      *           CLFILE.CL_PD_DLRECOURSE, 
      *           CLFILE.CL_PD_DLPARTRCAMT,
      *           CLFILE.CL_PD_ORIGPURAMT,
      *           CLFILE.CL_PD_PURBAL,
      *           CLFILE.CL_PD_DLRVPRIN_1,
      *           CLFILE.CL_PD_DLRVPRIN_2,
      *           CLFILE.CL_PD_DLRVPRIN_3,
      *           CLFILE.CL_PD_DLRVCHGS_1,
      *           CLFILE.CL_PD_DLRVCHGS_2,
      *           CLFILE.CL_PD_DLRVCHGS_3,
      *           CLFILE.CL_PD_DLPARVHELD,
      *           CLFILE.CL_PD_DLPARVPAID,
      *           CLFILE.CL_PD_DLDISC,
      *           CAST(CLFILE.CL_PD_LCPDTH_DATE
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_INTPDTH_DATE			  
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_CURBAL,
      *           CLFILE.CL_PD_INTBAL,
      *           CLFILE.CL_PD_LCBAL,
      *           CLFILE.CL_PD_OTHBAL,
      *           CLFILE.CL_PD_OT2BAL,
      *           CLFILE.CL_PD_LCPAID,
      *           CLFILE.CL_PD_LCPARTIALS,
      *           CLFILE.CL_PD_STMT_COUNTER,
      *           CLFILE.CL_PD_PRIOR_STMT_COUNTER,
      *           CLFILE.CL_PD_FROZEN_REASON, 
      *           CLFILE.CL_PD_COUPON_STATEMENT, 
      *           CLFILE.CL_PD_MONTHEND_BAL,
      *           CLFILE.CL_PD_ANTICTERM,
      *           CLFILE.CL_PD_ANTICERN_1,
      *           CLFILE.CL_PD_ANTICERN_2,
      *           CLFILE.CL_PD_ANTICERN_3,
      *           CLFILE.CL_PD_ANTICERN_4,
      *           CLFILE.CL_PD_ANTICERN_5,
      *           CLFILE.CL_PD_ANTICERN_6,
      *           CLFILE.CL_PD_ANTICERN_7,
      *           CLFILE.CL_PD_ANTICERN_8,
      *           CLFILE.CL_PD_ANTICERN_9,
      *           CLFILE.CL_PD_ANTICERN_10,
      *           CLFILE.CL_PD_ANTICADJ_1,
      *           CLFILE.CL_PD_ANTICADJ_2,
      *           CLFILE.CL_PD_ANTICADJ_3,
      *           CLFILE.CL_PD_ANTICADJ_4,
      *           CLFILE.CL_PD_ANTICADJ_5,
      *           CLFILE.CL_PD_ANTICADJ_6,
      *           CLFILE.CL_PD_ANTICADJ_7,
      *           CLFILE.CL_PD_ANTICADJ_8,
      *           CLFILE.CL_PD_ANTICADJ_9,
      *           CLFILE.CL_PD_ANTICADJ_10,
      *           CLFILE.CL_PD_UFERN_1,
      *           CLFILE.CL_PD_UFERN_2,
      *           CLFILE.CL_PD_UFERN_3,
      *           CLFILE.CL_PD_UFERN_4,
      *           CLFILE.CL_PD_UFERN_5,
      *           CLFILE.CL_PD_UFERN_6,
      *           CLFILE.CL_PD_UFERN_7,
      *           CLFILE.CL_PD_UFERN_8,
      *           CLFILE.CL_PD_UFERN_9,
      *           CLFILE.CL_PD_UFERN_10,
      *           CLFILE.CL_PD_ACCERN_1,
      *           CLFILE.CL_PD_ACCERN_2,
      *           CLFILE.CL_PD_ACCERN_3,
      *           CLFILE.CL_PD_ACCERN_4,
      *           CLFILE.CL_PD_ACCERN_5,
      *           CLFILE.CL_PD_ACCERN_6,
      *           CLFILE.CL_PD_ACCERN_7,
      *           CLFILE.CL_PD_ACCERN_8,
      *           CLFILE.CL_PD_ACCERN_9,
      *           CLFILE.CL_PD_ACCERN_10,
      *           CLFILE.CL_PD_EARN_ORDER_1,
      *           CLFILE.CL_PD_EARN_ORDER_2,
      *           CLFILE.CL_PD_EARN_ORDER_3,
      *           CLFILE.CL_PD_EARN_ORDER_4,
      *           CLFILE.CL_PD_EARN_ORDER_5,
      *           CLFILE.CL_PD_EARN_ORDER_6,
      *           CLFILE.CL_PD_EARN_ORDER_7,
      *           CLFILE.CL_PD_EARN_ORDER_8,
      *           CLFILE.CL_PD_EARN_ORDER_9,
      *           CLFILE.CL_PD_EARN_ORDER_10,
      *           CLFILE.CL_PD_LMO_PRIOR_INTEREST,
      *           CLFILE.CL_PD_PRIOR_UNEARNED,
      *           CLFILE.CL_PD_ERNDELQ_STOP,  
      *           CLFILE.CL_PD_NONACRERN,
      *           CLFILE.CL_PD_PRIOR_NONACRERN,
      *           CAST(CLFILE.CL_PD_ERN_IB_PDTH_DATE
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_ERN_IB_PRINBAL,
      *           CLFILE.CL_PD_ERN_IB_INTOWE,
      *           CLFILE.CL_PD_TOTERN,
      *           CLFILE.CL_PD_TOTOTH,
      *           CLFILE.CL_PD_TOTINT,
      *           CLFILE.CL_PD_TOTLCHG,
      *           CLFILE.CL_PD_TOTDEF,
      *           CLFILE.CL_PD_TOTSERACCT,
      *           CLFILE.CL_PD_LCWAIVERS,
      *           CLFILE.CL_PD_INTWAIVERS,
      *           CLFILE.CL_PD_TOTPAYMNTD,
      *           CLFILE.CL_PD_TOTEXCPAYMNTD,
      *           CLFILE.CL_PD_TOTNODEF,
      *           CLFILE.CL_PD_YTDNODEF,
      *           CLFILE.CL_PD_TOTNODEFACT,
      *           CLFILE.CL_PD_YTDNODEFACT,
      *           CLFILE.CL_PD_TOTNOALDEL,
      *           CLFILE.CL_PD_YTDNOALDEL,
      *           CLFILE.CL_PD_YTDINTPD,
      *           CLFILE.CL_PD_LYRINTPD,
      *           CLFILE.CL_PD_TOTINTPD,
      *           CLFILE.CL_PD_RECASTINT,
      *           CLFILE.CL_PD_RECASTPRIN,
      *           CLFILE.CL_PD_CL_REBATE, 
      *           CLFILE.CL_PD_AH_REBATE, 
      *           CLFILE.CL_PD_PP_REBATE, 
      *           CLFILE.CL_PD_INT_REBATE, 
      *           CLFILE.CL_PD_SC_REBATE, 
      *           CLFILE.CL_PD_MF_REBATE, 
      *           CLFILE.CL_PD_DF_REBATE, 
      *           CLFILE.CL_PD_EX_REBATE, 
      *           CLFILE.CL_PD_O1_REBATE, 
      *           CLFILE.CL_PD_O2_REBATE, 
      *           CLFILE.CL_PD_O3_REBATE, 
      *           CLFILE.CL_PD_O4_REBATE, 
      *           CLFILE.CL_PD_O5_REBATE, 
      *           CLFILE.CL_PD_O6_REBATE, 
      *           CLFILE.CL_PD_O7_REBATE, 
      *           CLFILE.CL_PD_UN_O7_REB, 
      *           CLFILE.CL_PD_UN_O8_REB, 
      *           CLFILE.CL_PD_UN_O9_REB, 
      *           CLFILE.CL_PD_UN_O10_REB, 
      *           CLFILE.CL_PD_PLREASON,  
      *           CLFILE.CL_PD_COLLECTOR, 
      *           CLFILE.CL_PD_ACTIONCD, 
      *           CLFILE.CL_PD_ACTIONCD2, 
      *           CLFILE.CL_PD_ATCODE, 
      *           CLFILE.CL_PD_RECODE, 
      *           CLFILE.CL_PD_ESCROW_FG, 
      *           CLFILE.CL_PD_PROMDAYS_PASTDUE,
      *           CLFILE.CL_PD_PROMID, 
      *           CAST(CLFILE.CL_PD_PROMDATE
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_PROMAMT,
      *           CAST(CLFILE.CL_PD_PROMDATE2
      *                                   AS VARCHAR(10)), 
      *           CAST(CLFILE.CL_PD_DATE_PAID_LAST		  
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_AMT_PAID_LAST,					  
      *           CLFILE.CL_PD_LASTCFORMNO,						  
      *           CAST(CLFILE.CL_PD_LASTCFORMDATE		  
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_DELCNT,							  
      *           CLFILE.CL_PD_DELCNT2,							  
      *           CLFILE.CL_PD_CREDITLIM,						  
      *           CLFILE.CL_PD_CRSCORE,							  
      *           CLFILE.CL_PD_LASTSFORMNO,						  
      *           CAST(CLFILE.CL_PD_LASTSFORMDATE		  
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_SOLCNT,
      *           CLFILE.CL_PD_MESSAGE, 
      *           CLFILE.CL_PD_OLDACCTNO,
      *           CLFILE.CL_PD_PRLNNO,
      *           CLFILE.CL_PD_PRBAL,
      *           CLFILE.CL_PD_PRINTDUE,
      *           CLFILE.CL_PD_PRFINCHGRB,
      *           CLFILE.CL_PD_PRINSRB,
      *           CLFILE.CL_PD_PRPOCD, 
      *           CAST(CLFILE.CL_PD_PRPODATE
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_PRDELINQAMT,
      *           CLFILE.CL_PD_PRLNAMT,
      *           CLFILE.CL_PD_PRPDUE_DAYS,
      *           CLFILE.CL_PD_PRDLDISC_UNEARN,
      *           CLFILE.CL_PD_PRCONPROFILE,
      *           CLFILE.CL_PD_SUM_ORIGPURAMT,
      *           CLFILE.CL_PD_SUM_LNAMT,
      *           CLFILE.CL_PD_SUM_FINCHG,
      *           CLFILE.CL_PD_SUM_INTCHG,
      *           CLFILE.CL_PD_SUM_ORGTERM,
      *           CLFILE.CL_PD_SUM_REGPYAMT,
      *           CLFILE.CL_PD_SUM_EXTCHG,
      *           CLFILE.CL_PD_SUM_SERCHG,
      *           CLFILE.CL_PD_SUM_MAINTFEE,
      *           CLFILE.CL_PD_UNITPER_CD, 
      *           CLFILE.CL_PD_UNITPER_FREQ,
      *           CLFILE.CL_PD_DAYS_IN_UNITPER,
      *           CLFILE.CL_PD_UNITPER_PER_YEAR,
      *           CLFILE.CL_PD_REPOCD, 
      *           CLFILE.CL_PD_BR_TFR_FG, 
      *           CLFILE.CL_PD_O3INS_DLPROC,
      *           CLFILE.CL_PD_INSPREM_DLADJ,
      *           CLFILE.CL_PD_PLAMT,
      *           CLFILE.CL_PD_MTD_PRIN,
      *           CLFILE.CL_PD_MTD_LC,
      *           CLFILE.CL_PD_MTD_INT,
      *           CLFILE.CL_PD_MTD_OT,
      *           CLFILE.CL_PD_MTD_EX,
      *           CLFILE.CL_PD_BNKRPT_CHAPTER,
      *           CLFILE.CL_PD_BMO_CON_STATUS, 
      *           CLFILE.CL_PD_BMO_REC_STATUS, 
      *           CLFILE.CL_PD_APP_NUMBER,
      *           CLFILE.CL_PD_GIFTNET, 
      *           CLFILE.CL_PD_GIFTNET_OUR_PO,
      *           CLFILE.CL_PD_PROC_DISBURSED, 
      *           CLFILE.CL_PD_NOODDPY,
      *           CLFILE.CL_PD_CONPROFILE_1,
      *           CLFILE.CL_PD_CONPROFILE_2,
      *           CLFILE.CL_PD_CONPROFILE_3,
      *           CLFILE.CL_PD_CONPROFILE_4,
      *           CLFILE.CL_PD_CONPROFILE_5,
      *           CLFILE.CL_PD_RECPROFILE_1,
      *           CLFILE.CL_PD_RECPROFILE_2,
      *           CLFILE.CL_PD_RECPROFILE_3,
      *           CLFILE.CL_PD_RECPROFILE_4,
      *           CLFILE.CL_PD_RECPROFILE_5,
      *           CLFILE.CL_PD_TRW_STATUS, 
      *           CLFILE.CL_PD_ORIGIN, 
      *           CLFILE.CL_PD_PO_TRCD, 
      *           CLFILE.CL_PD_NEXIS_CRSCORE,
      *           CLFILE.CL_PD_TRW_B_CONSUMER_IND, 
      *           CLFILE.CL_PD_PARVHELD_PAID_FG, 
      *           CLFILE.CL_PD_ACCT_FROZEN, 
      *           CLFILE.CL_PD_TRW_SPECIAL_CODE, 
      *           CLFILE.CL_PD_CREATION_STATUS, 
      *           CLFILE.CL_PD_TOTNOLCHG,
      *           CLFILE.CL_PD_LBOX, 
      *           CLFILE.CL_PD_EXTRA_PRIN_LFP,
      *           CLFILE.CL_PD_REMAIN_INT_SINCE_LFP,
      *           CLFILE.CL_PD_REMAIN_PRIN_SINCE_LFP,
      *           CLFILE.CL_PD_INTERIM_COUPON, 
      *           CLFILE.CL_PD_DLCOMM_O3_CMSI,
      *           CLFILE.CL_PD_DLCOMM_PP_CMSI,
      *           CLFILE.CL_PD_TRW_J_CONSUMER_IND, 
      *           CLFILE.CL_PD_ACH, 
      *           CLFILE.CL_PD_1099_C_FG, 
      *           CLFILE.CL_PD_TRW_COMPLIANCE_CODE, 
      *           CLFILE.CL_PD_UNWIND_REASON_CODE, 
      *           CLFILE.CL_PD_PBI_PBD_FG, 
      *           CLFILE.CL_PD_OLD_ACCT_FORMAT_FG, 
      *           CLFILE.CL_PD_APPROVED_BY_USER, 
      *           CLFILE.CL_PD_APPROVED_BY_CODE, 
      *           CAST(CLFILE.CL_PD_APPROVED_BY_DATE
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_APPROVED_BY_TIME,				  
      *           CAST(CLFILE.CL_PD_LTOUCH_DATE			  
      *                                   AS VARCHAR(10)), 
      *           CLFILE.CL_PD_LTOUCH_TIME,
      *           CLFILE.CL_PD_HOLD_COLLATERAL, 
      *           CLFILE.CL_PD_DLPAID, 
      *           CLFILE.CL_PD_CKNO,
      *           CLFILE.CL_PD_FREE1, 
      *           CLFILE.CL_PD_FREE2, 
      *           CLFILE.CL_PD_FREE_SPACE_FILLER, 
      *           CLFILE.CL_PD_POSTING_BR,
      *           CLFILE.CL_PD_OTHBR_FG, 
      *           CLFILE.CL_PD_APPROVAL_LIMIT,
      *           CLFILE.CL_PD_APPROVAL_USERID, 
      *           CLFILE.CL_PD_MIN_CRED_SCORE,
      *           CLFILE.CL_PD_CRED_SCORE_USERID,  
      *           CLFILE.CL_PD_FINAMT,
      *           CLFILE.CL_PD_PROCEEDS_FG, 
      *           CLFILE.CL_PD_POINTS_FG, 
      *           CLFILE.CL_PD_OVRDRATE_FG, 
      *           CLFILE.CL_PD_INS_FG_1, 
      *           CLFILE.CL_PD_INS_FG_2, 
      *           CLFILE.CL_PD_INS_FG_3, 
      *           CLFILE.CL_PD_INS_FG_4, 
      *           CLFILE.CL_PD_INS_FG_5, 
      *           CLFILE.CL_PD_INS_FG_6, 
      *           CLFILE.CL_PD_INS_FG_7, 
      *           CLFILE.CL_PD_INS_FG_8, 
      *           CLFILE.CL_PD_INS_FG_9, 
      *           CLFILE.CL_PD_INS_FG_10, 
      *           CLFILE.CL_PD_SERCHG_FG, 
      *           CLFILE.CL_PD_MAINTFEE_FG, 
      *           CLFILE.CL_PD_EXTCHG_FG, 
      *           CLFILE.CL_PD_REGPYAMT_FG, 
      *           CLFILE.CL_PD_FEECD_1, 
      *           CLFILE.CL_PD_FEECD_2, 
      *           CLFILE.CL_PD_FEECD_3, 
      *           CLFILE.CL_PD_FEECD_4, 
      *           CLFILE.CL_PD_FEECD_5, 
      *           CLFILE.CL_PD_FEECD_6, 
      *           CLFILE.CL_PD_FEECD_7, 
      *           CLFILE.CL_PD_FEECD_8, 
      *           CLFILE.CL_PD_FEECD_9, 
      *           CLFILE.CL_PD_FEECD_10, 
      *           CLFILE.CL_PD_FEECD_11, 
      *           CLFILE.CL_PD_FEECD_12, 
      *           CLFILE.CL_PD_FEECD_13, 
      *           CLFILE.CL_PD_FEECD_14, 
      *           CLFILE.CL_PD_FEECD_15, 
      *           CLFILE.CL_PD_FEECD_16, 
      *           CLFILE.CL_PD_FEECD_17, 
      *           CLFILE.CL_PD_FEECD_18, 
      *           CLFILE.CL_PD_FEECD_19, 
      *           CLFILE.CL_PD_FEECD_20, 
      *           CLFILE.CL_PD_PRPAID_AHEAD,
      *           CLFILE.CL_PD_RESTRICT_ADJ,
      *           CLFILE.CL_PD_DUAL_LOAN_ADJ,
      *           CLFILE.CL_PD_CAN_LOAN_MAX,
      *           CLFILE.CL_PD_PERDOWN,
      *           CLFILE.CL_PD_SELLPR_FG, 
      *           CLFILE.CL_PD_DOWNPY_FG, 
      *           CLFILE.CL_PD_PERDOWN_FG, 
      *           CLFILE.CL_PD_SELL_DOWN_FG, 
      *           CLFILE.CL_PD_BALLOON_FG, 
      *           CLFILE.CL_PD_BALLOON_LAST_FG, 
      *           CLFILE.CL_PD_FEECD_FG_1, 
      *           CLFILE.CL_PD_FEECD_FG_2, 
      *           CLFILE.CL_PD_INS_TERM_SW_1, 
      *           CLFILE.CL_PD_INS_TERM_SW_2, 
      *           CLFILE.CL_PD_INS_TERM_SW_3, 
      *           CLFILE.CL_PD_INS_TERM_SW_4, 
      *           CLFILE.CL_PD_INS_TERM_SW_5, 
      *           CLFILE.CL_PD_INS_TERM_SW_6, 
      *           CLFILE.CL_PD_INS_TERM_SW_7, 
      *           CLFILE.CL_PD_INS_TERM_SW_8, 
      *           CLFILE.CL_PD_INS_TERM_SW_9, 
      *           CLFILE.CL_PD_INS_TERM_SW_10, 
      *           CLFILE.CL_PD_COVERAGE_SW_1, 
      *           CLFILE.CL_PD_COVERAGE_SW_2, 
      *           CLFILE.CL_PD_COVERAGE_SW_3, 
      *           CLFILE.CL_PD_COVERAGE_SW_4, 
      *           CLFILE.CL_PD_COVERAGE_SW_5, 
      *           CLFILE.CL_PD_COVERAGE_SW_6, 
      *           CLFILE.CL_PD_COVERAGE_SW_7, 
      *           CLFILE.CL_PD_COVERAGE_SW_8, 
      *           CLFILE.CL_PD_COVERAGE_SW_9, 
      *           CLFILE.CL_PD_COVERAGE_SW_10, 
      *           CLFILE.CL_PD_PREMIUM_SW_1, 
      *           CLFILE.CL_PD_PREMIUM_SW_2, 
      *           CLFILE.CL_PD_PREMIUM_SW_3, 
      *           CLFILE.CL_PD_PREMIUM_SW_4, 
      *           CLFILE.CL_PD_PREMIUM_SW_5, 
      *           CLFILE.CL_PD_PREMIUM_SW_6, 
      *           CLFILE.CL_PD_PREMIUM_SW_7, 
      *           CLFILE.CL_PD_PREMIUM_SW_8, 
      *           CLFILE.CL_PD_PREMIUM_SW_9, 
      *           CLFILE.CL_PD_PREMIUM_SW_10, 
      *           CLFILE.CL_PD_DLDISC_FG, 
      *           CLFILE.CL_PD_DLRVPRIN_FG_1, 
      *           CLFILE.CL_PD_DLRVPRIN_FG_2, 
      *           CLFILE.CL_PD_DLRVPRIN_FG_3, 
      *           CLFILE.CL_PD_DLRVCHGS_FG_1, 
      *           CLFILE.CL_PD_DLRVCHGS_FG_2, 
      *           CLFILE.CL_PD_DLRVCHGS_FG_3, 
      *           CLFILE.CL_PD_DLPARVHELD_FG, 
      *           CLFILE.CL_PD_DLPARVPAID_FG, 
      *           CLFILE.CL_PD_ONE_MTH_NO_DAY_EXT_SW, 
      *           CLFILE.CL_PD_ASSIGN_RN_ACCTNO, 
      *           CLFILE.CL_PD_APR_FINAMT,
      *           CLFILE.CL_PD_DLCOMM_PP_CMSI_FG, 
      *           CLFILE.CL_PD_DLCOMM_O3_CMSI_FG, 
      *           CLFILE.CL_PD_FILLER_TO_MATCH_LN, 
      *           CLFILE.CL_PD2_ID, 
      *           CLFILE.CL_PD2_BRANCH,
      *           CLFILE.CL_PD2_BWNAME,
      *           ISNULL(SPAFILE.SPA_ORGST,'%'),
      *           ISNULL(SPAFILE.SPA_SPRCLASS, 0),
      *           ISNULL(SPAFILE.SPA_SUBCLASS, 0),
      *           ISNULL(SPAFILE.SPA_LAWCODE, 0),
      *           ISNULL(SPAFILE.SPA_MFFRMLA, 0),
      *           ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,' '),
      *           ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,' ')
      *  
      *      FROM DBO.CLFILE  CLFILE
      *      LEFT OUTER JOIN
      *           DBO.SPAFILE SPAFILE
      *        ON ( CLFILE.CL_PD_ORGST       = SPAFILE.SPA_ORGST
      *       AND   CLFILE.CL_PD_SPRCLASS    = SPAFILE.SPA_SPRCLASS
      *       AND   CLFILE.CL_PD_SUBCLASS    = SPAFILE.SPA_SUBCLASS
      *       AND   CLFILE.CL_PD_LAWCODE     = SPAFILE.SPA_LAWCODE )
      *  
      *     WHERE ( CLFILE.CL_GROUP_ID BETWEEN @QCL1_WBEG_GROUP
      *                                    AND @QCL1_WEND_GROUP
      *       AND   CLFILE.CL_CAN_NO   BETWEEN @QCL1_WBEG_CAN_NO    
      *                                    AND @QCL1_WEND_CAN_NO )              


           EXEC SQL SET CONNECTION CL_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CL1_CURSOR CURSOR FOR
                :CL1-RETURN-CODE = EXEC SSP_CLEXTR_CLFILE_SELECT
              ( :QCL1-WBEG-GROUP,
                :QCL1-WEND-GROUP, 
                :QCL1-WBEG-CAN-NO, 
                :QCL1-WEND-CAN-NO )                    
           END-EXEC.
   
           EXEC SQL
               OPEN CL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCL1-WBEG-GROUP
                          QCL1-WEND-GROUP
                          QCL1-WBEG-CAN-NO
                          QCL1-WEND-CAN-NO.

           MOVE STAT---GOOD TO CL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------

       READ-CL1-FILE-NEXT. *> EMBEDDED - CL,SPA
           PERFORM READ-IT.
           MOVE CL-PATH-SQL TO E-FILE.
           MOVE CL1-KEY TO E-KEYX.
           INITIALIZE QCL-REC.
           INITIALIZE QSPA-REC.

           EXEC SQL SET CONNECTION CL_CONNECT END-EXEC.

           IF ( CL1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH CL1_CURSOR 
                  INTO 
                   :QCL-GROUP-ID, 
                   :QCL-CAN-NO, 
                   :QCL-DESC, 
                   :QCL-GIFTNET, 
                   :QCL-PD-ID, 
                   :QCL-PD-BRANCH,
                   :QCL-PD-SOC-SEC,
                   :QCL-PD-OWNBR,
                   :QCL-PD-ACCTNO,
                   :QCL-PD-CLASS,
                   :QCL-PD-SUBCLASS,
                   :QCL-PD-LAWCODE,
                   :QCL-PD-SSNO1,
                   :QCL-PD-MAKERCD1,
                   :QCL-PD-SSNO2,
                   :QCL-PD-MAKERCD2,
                   :QCL-PD-SSNO3,
                   :QCL-PD-MAKERCD3,
                   :QCL-PD-SSNO4,
                   :QCL-PD-MAKERCD4,
                   :QCL-PD-ORGST,
                   :QCL-PD-ORGBR,
                   :QCL-PD-OWNST,
                   :QCL-PD-SPRCLASS,
                   :QCL-PD-CONVERCD, 
                   :QCL-PD-MIDSTREAM, 
                   :QCL-PD-PURCD, 
                   :QCL-PD-SRCD, 
                   :QCL-PD-COLLCD, 
                   :QCL-PD-COLLID, 
                   :QCL-PD-POCD, 
                   :QCL-PD-PLCD, 
                   :QCL-PD-PLTYPE, 
                   :QCL-PD-CONFIDENTCD, 
                   :QCL-PD-ALLOTCD,
                   :QCL-PD-RECASTCD, 
                   :QCL-PD-SKIP-TRW, 
                   :QCL-PD-STATUSFG, 
                   :QCL-PD-SMALLOAN, 
                   :QCL-PD-SALEFIN,  
                   :QCL-PD-SERVACCT, 
                   :QCL-PD-LOANTYPE, 
                   :QCL-PD-ADDON-FG, 
                   :QCL-PD-PAY-SCHLD-FG, 
                   :QCL-PD-LC-CODE,
                   :QCL-PD-CAN-LOAN-NO, 
                   :QCL-PD-STATFLAGS, 
                   :QCL-PD-ENTDATE, 
                   :QCL-PD-LOANDATE, 
                   :QCL-PD-PURDATE, 
                   :QCL-PD-INTDATE, 
                   :QCL-PD-CASHDATE, 
                   :QCL-PD-FILEDATE, 
                   :QCL-PD-RESCISSION-DATE, 
                   :QCL-PD-PART-RECOURSE-DATE, 
                   :QCL-PD-ORIG-1STPYDATE, 
                   :QCL-PD-1STPYDATE, 
                   :QCL-PD-JDDATE, 
                   :QCL-PD-PLDATE, 
                   :QCL-PD-PL-TRDATE, 
                   :QCL-PD-ACCELDATE, 
                   :QCL-PD-REDUDATE, 
                   :QCL-PD-POFFDATE, 
                   :QCL-PD-PO-TRDATE, 
                   :QCL-PD-LEGALDATE, 
                   :QCL-PD-BNKRPTDATE, 
                   :QCL-PD-BNKRPT-DISMISS-DATE, 
                   :QCL-PD-BNKRPT-DISCHARGE-DATE, 
                   :QCL-PD-BNKRPT-REAFFIRM-DATE, 
                   :QCL-PD-DEBTAIDATE, 
                   :QCL-PD-REPODATE, 
                   :QCL-PD-REDEEMDATE, 
                   :QCL-PD-SOLDDATE, 
                   :QCL-PD-INS-CONFIRM-DATE, 
                   :QCL-PD-PRIOR-RN-FOR10-DATE, 
                   :QCL-PD-LAST-RN-FOR10-DATE, 
                   :QCL-PD-TRW-DOFD-DATE, 
                   :QCL-PD-LAST-PYPA-DATE, 
                   :QCL-PD-BT-TRDATE, 
                   :QCL-PD-METRO2-LAST-PMT-DATE, 
                   :QCL-PD-FOLLOWUP-DATE, 
                   :QCL-PD-LAST-ACCRUAL-DATE, 
                   :QCL-PD-APRATE,
                   :QCL-PD-EFFRATE,
                   :QCL-PD-SMPRATE,
                   :QCL-PD-YLDRATE,
                   :QCL-PD-OVRDRATE,
                   :QCL-PD-ENDORATE,
                   :QCL-PD-JDRATE,
                   :QCL-PD-ACCRATE,
                   :QCL-PD-REDURATE,
                   :QCL-PD-CONRATE,
                   :QCL-PD-SERVPER,
                   :QCL-PD-SELLPR,
                   :QCL-PD-DOWNPY,
                   :QCL-PD-TRADEIN,
                   :QCL-PD-LNAMT,
                   :QCL-PD-FINCHG,
                   :QCL-PD-INTCHG,
                   :QCL-PD-SERCHG,
                   :QCL-PD-EXTCHG,
                   :QCL-PD-MAINTFEE,
                   :QCL-PD-POINTS,
                   :QCL-PD-PROCEEDS,
                   :QCL-PD-ORGTERM,
                   :QCL-PD-1STPYAMT,
                   :QCL-PD-NOREGPY,
                   :QCL-PD-REGPYAMT,
                   :QCL-PD-LASTPYAMT,
                   :QCL-PD-PROCASH,
                   :QCL-PD-TILLNO,
                   :QCL-PD-STATE-FLAG,
                   :QCL-PD-STATE-WORKER,
                   :QCL-PD-STATE-WORKER2,
                   :QCL-PD-FEECODE-1, 
                   :QCL-PD-FEECODE-2, 
                   :QCL-PD-FEECODE-3, 
                   :QCL-PD-FEECODE-4, 
                   :QCL-PD-FEECODE-5, 
                   :QCL-PD-FEECODE-6, 
                   :QCL-PD-FEECODE-7, 
                   :QCL-PD-FEECODE-8, 
                   :QCL-PD-FEECODE-9, 
                   :QCL-PD-FEECODE-10, 
                   :QCL-PD-FEECODE-11, 
                   :QCL-PD-FEECODE-12, 
                   :QCL-PD-FEECODE-13, 
                   :QCL-PD-FEECODE-14, 
                   :QCL-PD-FEECODE-15, 
                   :QCL-PD-FEECODE-16, 
                   :QCL-PD-FEECODE-17, 
                   :QCL-PD-FEECODE-18, 
                   :QCL-PD-FEECODE-19, 
                   :QCL-PD-FEECODE-20, 
                   :QCL-PD-FEEAMT-1,
                   :QCL-PD-FEEAMT-2,
                   :QCL-PD-FEEAMT-3,
                   :QCL-PD-FEEAMT-4,
                   :QCL-PD-FEEAMT-5,
                   :QCL-PD-FEEAMT-6,
                   :QCL-PD-FEEAMT-7,
                   :QCL-PD-FEEAMT-8,
                   :QCL-PD-FEEAMT-9,
                   :QCL-PD-FEEAMT-10,
                   :QCL-PD-FEEAMT-11,
                   :QCL-PD-FEEAMT-12,
                   :QCL-PD-FEEAMT-13,
                   :QCL-PD-FEEAMT-14,
                   :QCL-PD-FEEAMT-15,
                   :QCL-PD-FEEAMT-16,
                   :QCL-PD-FEEAMT-17,
                   :QCL-PD-FEEAMT-18,
                   :QCL-PD-FEEAMT-19,
                   :QCL-PD-FEEAMT-20,
                   :QCL-PD-FEEOURS-1, 
                   :QCL-PD-FEEOURS-2, 
                   :QCL-PD-FEEOURS-3, 
                   :QCL-PD-FEEOURS-4, 
                   :QCL-PD-FEEOURS-5, 
                   :QCL-PD-FEEOURS-6, 
                   :QCL-PD-FEEOURS-7, 
                   :QCL-PD-FEEOURS-8, 
                   :QCL-PD-FEEOURS-9, 
                   :QCL-PD-FEEOURS-10, 
                   :QCL-PD-FEEOURS-11, 
                   :QCL-PD-FEEOURS-12, 
                   :QCL-PD-FEEOURS-13, 
                   :QCL-PD-FEEOURS-14, 
                   :QCL-PD-FEEOURS-15, 
                   :QCL-PD-FEEOURS-16, 
                   :QCL-PD-FEEOURS-17, 
                   :QCL-PD-FEEOURS-18, 
                   :QCL-PD-FEEOURS-19, 
                   :QCL-PD-FEEOURS-20, 
                   :QCL-PD-FEEFINCHG-1, 
                   :QCL-PD-FEEFINCHG-2, 
                   :QCL-PD-FEEFINCHG-3, 
                   :QCL-PD-FEEFINCHG-4, 
                   :QCL-PD-FEEFINCHG-5, 
                   :QCL-PD-FEEFINCHG-6, 
                   :QCL-PD-FEEFINCHG-7, 
                   :QCL-PD-FEEFINCHG-8, 
                   :QCL-PD-FEEFINCHG-9, 
                   :QCL-PD-FEEFINCHG-10, 
                   :QCL-PD-FEEFINCHG-11, 
                   :QCL-PD-FEEFINCHG-12, 
                   :QCL-PD-FEEFINCHG-13, 
                   :QCL-PD-FEEFINCHG-14, 
                   :QCL-PD-FEEFINCHG-15, 
                   :QCL-PD-FEEFINCHG-16, 
                   :QCL-PD-FEEFINCHG-17, 
                   :QCL-PD-FEEFINCHG-18, 
                   :QCL-PD-FEEFINCHG-19, 
                   :QCL-PD-FEEFINCHG-20, 
                   :QCL-PD-INSURED,
                   :QCL-PD-INSTYPES,
                   :QCL-PD-INSELIGIBLE,
                   :QCL-PD-INSCOMP-1,
                   :QCL-PD-INSCOMP-2,
                   :QCL-PD-INSCOMP-3,
                   :QCL-PD-INSCOMP-4,
                   :QCL-PD-INSCOMP-5,
                   :QCL-PD-INSCOMP-6,
                   :QCL-PD-INSCOMP-7,
                   :QCL-PD-INSCOMP-8,
                   :QCL-PD-INSCOMP-9,
                   :QCL-PD-INSCOMP-10,
                   :QCL-PD-INSOURS-1, 
                   :QCL-PD-INSOURS-2, 
                   :QCL-PD-INSOURS-3, 
                   :QCL-PD-INSOURS-4, 
                   :QCL-PD-INSOURS-5, 
                   :QCL-PD-INSOURS-6, 
                   :QCL-PD-INSOURS-7, 
                   :QCL-PD-INSOURS-8, 
                   :QCL-PD-INSOURS-9, 
                   :QCL-PD-INSOURS-10, 
                   :QCL-PD-INSEFF-DATE-1, 
                   :QCL-PD-INSEFF-DATE-2, 
                   :QCL-PD-INSEFF-DATE-3, 
                   :QCL-PD-INSEFF-DATE-4, 
                   :QCL-PD-INSEFF-DATE-5, 
                   :QCL-PD-INSEFF-DATE-6, 
                   :QCL-PD-INSEFF-DATE-7, 
                   :QCL-PD-INSEFF-DATE-8, 
                   :QCL-PD-INSEFF-DATE-9, 
                   :QCL-PD-INSEFF-DATE-10, 
                   :QCL-PD-INSEXP-DATE-1, 
                   :QCL-PD-INSEXP-DATE-2, 
                   :QCL-PD-INSEXP-DATE-3, 
                   :QCL-PD-INSEXP-DATE-4, 
                   :QCL-PD-INSEXP-DATE-5, 
                   :QCL-PD-INSEXP-DATE-6, 
                   :QCL-PD-INSEXP-DATE-7, 
                   :QCL-PD-INSEXP-DATE-8, 
                   :QCL-PD-INSEXP-DATE-9, 
                   :QCL-PD-INSEXP-DATE-10, 
                   :QCL-PD-INSPREM-1,
                   :QCL-PD-INSPREM-2,
                   :QCL-PD-INSPREM-3,
                   :QCL-PD-INSPREM-4,
                   :QCL-PD-INSPREM-5,
                   :QCL-PD-INSPREM-6,
                   :QCL-PD-INSPREM-7,
                   :QCL-PD-INSPREM-8,
                   :QCL-PD-INSPREM-9,
                   :QCL-PD-INSPREM-10,
                   :QCL-PD-INSCOVR-1,
                   :QCL-PD-INSCOVR-2,
                   :QCL-PD-INSCOVR-3,
                   :QCL-PD-INSCOVR-4,
                   :QCL-PD-INSCOVR-5,
                   :QCL-PD-INSCOVR-6,
                   :QCL-PD-INSCOVR-7,
                   :QCL-PD-INSCOVR-8,
                   :QCL-PD-INSCOVR-9,
                   :QCL-PD-INSCOVR-10,
                   :QCL-PD-INSCOMM-1,
                   :QCL-PD-INSCOMM-2,
                   :QCL-PD-INSCOMM-3,
                   :QCL-PD-INSCOMM-4,
                   :QCL-PD-INSCOMM-5,
                   :QCL-PD-INSCOMM-6,
                   :QCL-PD-INSCOMM-7,
                   :QCL-PD-INSCOMM-8,
                   :QCL-PD-INSCOMM-9,
                   :QCL-PD-INSCOMM-10,
                   :QCL-PD-INSANTICERN-1,
                   :QCL-PD-INSANTICERN-2,
                   :QCL-PD-INSANTICERN-3,
                   :QCL-PD-INSANTICERN-4,
                   :QCL-PD-INSANTICERN-5,
                   :QCL-PD-INSANTICERN-6,
                   :QCL-PD-INSANTICERN-7,
                   :QCL-PD-INSANTICERN-8,
                   :QCL-PD-INSANTICERN-9,
                   :QCL-PD-INSANTICERN-10,
                   :QCL-PD-INSANTICADJ-1,
                   :QCL-PD-INSANTICADJ-2,
                   :QCL-PD-INSANTICADJ-3,
                   :QCL-PD-INSANTICADJ-4,
                   :QCL-PD-INSANTICADJ-5,
                   :QCL-PD-INSANTICADJ-6,
                   :QCL-PD-INSANTICADJ-7,
                   :QCL-PD-INSANTICADJ-8,
                   :QCL-PD-INSANTICADJ-9,
                   :QCL-PD-INSANTICADJ-10,
                   :QCL-PD-INSUFERN-1,
                   :QCL-PD-INSUFERN-2,
                   :QCL-PD-INSUFERN-3,
                   :QCL-PD-INSUFERN-4,
                   :QCL-PD-INSUFERN-5,
                   :QCL-PD-INSUFERN-6,
                   :QCL-PD-INSUFERN-7,
                   :QCL-PD-INSUFERN-8,
                   :QCL-PD-INSUFERN-9,
                   :QCL-PD-INSUFERN-10,
                   :QCL-PD-INSACCERN-1,
                   :QCL-PD-INSACCERN-2,
                   :QCL-PD-INSACCERN-3,
                   :QCL-PD-INSACCERN-4,
                   :QCL-PD-INSACCERN-5,
                   :QCL-PD-INSACCERN-6,
                   :QCL-PD-INSACCERN-7,
                   :QCL-PD-INSACCERN-8,
                   :QCL-PD-INSACCERN-9,
                   :QCL-PD-INSACCERN-10,
                   :QCL-PD-INSEARN-ORDER-1,
                   :QCL-PD-INSEARN-ORDER-2,
                   :QCL-PD-INSEARN-ORDER-3,
                   :QCL-PD-INSEARN-ORDER-4,
                   :QCL-PD-INSEARN-ORDER-5,
                   :QCL-PD-INSEARN-ORDER-6,
                   :QCL-PD-INSEARN-ORDER-7,
                   :QCL-PD-INSEARN-ORDER-8,
                   :QCL-PD-INSEARN-ORDER-9,
                   :QCL-PD-INSEARN-ORDER-10,
                   :QCL-PD-INSFINCHG-1, 
                   :QCL-PD-INSFINCHG-2, 
                   :QCL-PD-INSFINCHG-3, 
                   :QCL-PD-INSFINCHG-4, 
                   :QCL-PD-INSFINCHG-5, 
                   :QCL-PD-INSFINCHG-6, 
                   :QCL-PD-INSFINCHG-7, 
                   :QCL-PD-INSFINCHG-8, 
                   :QCL-PD-INSFINCHG-9, 
                   :QCL-PD-INSFINCHG-10, 
                   :QCL-PD-PY-YYYYMM-RECDEL,
                   :QCL-PD-MTD-PAYS-RECDEL,
                   :QCL-PD-GARNISH-EXTR, 
                   :QCL-PD-SHIP-SAME-FG, 
                   :QCL-PD-AD-D-INS-COVR,
                   :QCL-PD-TOT-PARTIAL-CHGOFF,
                   :QCL-PD-LPO, 
                   :QCL-PD-REP-CD, 
                   :QCL-PD-EMP-ID,
                   :QCL-PD-PROCESS-CD, 
                   :QCL-PD-WCBC-CREDITLIM,
                   :QCL-PD-FOLLOWUP-CD, 
                   :QCL-PD-MIP-FG, 
                   :QCL-PD-MIP-MTD-CLAHPAID,
                   :QCL-PD-MIP-CLPDTH-DATE, 
                   :QCL-PD-MIP-TOT-CLPAID,  
                   :QCL-PD-MIP-AHPDTH-DATE, 
                   :QCL-PD-MIP-TOT-AHPAID,
                   :QCL-PD-IL-GROSS-INCOME,
                   :QCL-PD-IL-GROSS-INCOME2,
                   :QCL-PD-RESCISSION, 
                   :QCL-PD-INTERIM-TRW, 
                   :QCL-PD-DLNO,
                   :QCL-PD-DLPOOL,
                   :QCL-PD-DLRECOURSE, 
                   :QCL-PD-DLPARTRCAMT,
                   :QCL-PD-ORIGPURAMT,
                   :QCL-PD-PURBAL,
                   :QCL-PD-DLRVPRIN-1,
                   :QCL-PD-DLRVPRIN-2,
                   :QCL-PD-DLRVPRIN-3,
                   :QCL-PD-DLRVCHGS-1,
                   :QCL-PD-DLRVCHGS-2,
                   :QCL-PD-DLRVCHGS-3,
                   :QCL-PD-DLPARVHELD,
                   :QCL-PD-DLPARVPAID,
                   :QCL-PD-DLDISC,
                   :QCL-PD-LCPDTH-DATE, 
                   :QCL-PD-INTPDTH-DATE, 
                   :QCL-PD-CURBAL,
                   :QCL-PD-INTBAL,
                   :QCL-PD-LCBAL,
                   :QCL-PD-OTHBAL,
                   :QCL-PD-OT2BAL,
                   :QCL-PD-LCPAID,
                   :QCL-PD-LCPARTIALS,
                   :QCL-PD-STMT-COUNTER,
                   :QCL-PD-PRIOR-STMT-COUNTER,
                   :QCL-PD-FROZEN-REASON, 
                   :QCL-PD-COUPON-STATEMENT, 
                   :QCL-PD-MONTHEND-BAL,
                   :QCL-PD-ANTICTERM,
                   :QCL-PD-ANTICERN-1,
                   :QCL-PD-ANTICERN-2,
                   :QCL-PD-ANTICERN-3,
                   :QCL-PD-ANTICERN-4,
                   :QCL-PD-ANTICERN-5,
                   :QCL-PD-ANTICERN-6,
                   :QCL-PD-ANTICERN-7,
                   :QCL-PD-ANTICERN-8,
                   :QCL-PD-ANTICERN-9,
                   :QCL-PD-ANTICERN-10,
                   :QCL-PD-ANTICADJ-1,
                   :QCL-PD-ANTICADJ-2,
                   :QCL-PD-ANTICADJ-3,
                   :QCL-PD-ANTICADJ-4,
                   :QCL-PD-ANTICADJ-5,
                   :QCL-PD-ANTICADJ-6,
                   :QCL-PD-ANTICADJ-7,
                   :QCL-PD-ANTICADJ-8,
                   :QCL-PD-ANTICADJ-9,
                   :QCL-PD-ANTICADJ-10,
                   :QCL-PD-UFERN-1,
                   :QCL-PD-UFERN-2,
                   :QCL-PD-UFERN-3,
                   :QCL-PD-UFERN-4,
                   :QCL-PD-UFERN-5,
                   :QCL-PD-UFERN-6,
                   :QCL-PD-UFERN-7,
                   :QCL-PD-UFERN-8,
                   :QCL-PD-UFERN-9,
                   :QCL-PD-UFERN-10,
                   :QCL-PD-ACCERN-1,
                   :QCL-PD-ACCERN-2,
                   :QCL-PD-ACCERN-3,
                   :QCL-PD-ACCERN-4,
                   :QCL-PD-ACCERN-5,
                   :QCL-PD-ACCERN-6,
                   :QCL-PD-ACCERN-7,
                   :QCL-PD-ACCERN-8,
                   :QCL-PD-ACCERN-9,
                   :QCL-PD-ACCERN-10,
                   :QCL-PD-EARN-ORDER-1,
                   :QCL-PD-EARN-ORDER-2,
                   :QCL-PD-EARN-ORDER-3,
                   :QCL-PD-EARN-ORDER-4,
                   :QCL-PD-EARN-ORDER-5,
                   :QCL-PD-EARN-ORDER-6,
                   :QCL-PD-EARN-ORDER-7,
                   :QCL-PD-EARN-ORDER-8,
                   :QCL-PD-EARN-ORDER-9,
                   :QCL-PD-EARN-ORDER-10,
                   :QCL-PD-LMO-PRIOR-INTEREST,
                   :QCL-PD-PRIOR-UNEARNED,
                   :QCL-PD-ERNDELQ-STOP,  
                   :QCL-PD-NONACRERN,
                   :QCL-PD-PRIOR-NONACRERN,
                   :QCL-PD-ERN-IB-PDTH-DATE, 
                   :QCL-PD-ERN-IB-PRINBAL,
                   :QCL-PD-ERN-IB-INTOWE,
                   :QCL-PD-TOTERN,
                   :QCL-PD-TOTOTH,
                   :QCL-PD-TOTINT,
                   :QCL-PD-TOTLCHG,
                   :QCL-PD-TOTDEF,
                   :QCL-PD-TOTSERACCT,
                   :QCL-PD-LCWAIVERS,
                   :QCL-PD-INTWAIVERS,
                   :QCL-PD-TOTPAYMNTD,
                   :QCL-PD-TOTEXCPAYMNTD,
                   :QCL-PD-TOTNODEF,
                   :QCL-PD-YTDNODEF,
                   :QCL-PD-TOTNODEFACT,
                   :QCL-PD-YTDNODEFACT,
                   :QCL-PD-TOTNOALDEL,
                   :QCL-PD-YTDNOALDEL,
                   :QCL-PD-YTDINTPD,
                   :QCL-PD-LYRINTPD,
                   :QCL-PD-TOTINTPD,
                   :QCL-PD-RECASTINT,
                   :QCL-PD-RECASTPRIN,
                   :QCL-PD-CL-REBATE, 
                   :QCL-PD-AH-REBATE, 
                   :QCL-PD-PP-REBATE, 
                   :QCL-PD-INT-REBATE, 
                   :QCL-PD-SC-REBATE, 
                   :QCL-PD-MF-REBATE, 
                   :QCL-PD-DF-REBATE, 
                   :QCL-PD-EX-REBATE, 
                   :QCL-PD-O1-REBATE, 
                   :QCL-PD-O2-REBATE, 
                   :QCL-PD-O3-REBATE, 
                   :QCL-PD-O4-REBATE, 
                   :QCL-PD-O5-REBATE, 
                   :QCL-PD-O6-REBATE, 
                   :QCL-PD-O7-REBATE, 
                   :QCL-PD-UN-O7-REB, 
                   :QCL-PD-UN-O8-REB, 
                   :QCL-PD-UN-O9-REB, 
                   :QCL-PD-UN-O10-REB, 
                   :QCL-PD-PLREASON,  
                   :QCL-PD-COLLECTOR, 
                   :QCL-PD-ACTIONCD, 
                   :QCL-PD-ACTIONCD2, 
                   :QCL-PD-ATCODE, 
                   :QCL-PD-RECODE, 
                   :QCL-PD-ESCROW-FG, 
                   :QCL-PD-PROMDAYS-PASTDUE,
                   :QCL-PD-PROMID, 
                   :QCL-PD-PROMDATE, 
                   :QCL-PD-PROMAMT,
                   :QCL-PD-PROMDATE2, 
                   :QCL-PD-DATE-PAID-LAST, 
                   :QCL-PD-AMT-PAID-LAST,  
                   :QCL-PD-LASTCFORMNO,  
                   :QCL-PD-LASTCFORMDATE, 
                   :QCL-PD-DELCNT,  
                   :QCL-PD-DELCNT2,  
                   :QCL-PD-CREDITLIM, 
                   :QCL-PD-CRSCORE,  
                   :QCL-PD-LASTSFORMNO,  
                   :QCL-PD-LASTSFORMDATE, 
                   :QCL-PD-SOLCNT,
                   :QCL-PD-MESSAGE, 
                   :QCL-PD-OLDACCTNO,
                   :QCL-PD-PRLNNO,
                   :QCL-PD-PRBAL,
                   :QCL-PD-PRINTDUE,
                   :QCL-PD-PRFINCHGRB,
                   :QCL-PD-PRINSRB,
                   :QCL-PD-PRPOCD, 
                   :QCL-PD-PRPODATE, 
                   :QCL-PD-PRDELINQAMT,
                   :QCL-PD-PRLNAMT,
                   :QCL-PD-PRPDUE-DAYS,
                   :QCL-PD-PRDLDISC-UNEARN,
                   :QCL-PD-PRCONPROFILE,
                   :QCL-PD-SUM-ORIGPURAMT,
                   :QCL-PD-SUM-LNAMT,
                   :QCL-PD-SUM-FINCHG,
                   :QCL-PD-SUM-INTCHG,
                   :QCL-PD-SUM-ORGTERM,
                   :QCL-PD-SUM-REGPYAMT,
                   :QCL-PD-SUM-EXTCHG,
                   :QCL-PD-SUM-SERCHG,
                   :QCL-PD-SUM-MAINTFEE,
                   :QCL-PD-UNITPER-CD, 
                   :QCL-PD-UNITPER-FREQ,
                   :QCL-PD-DAYS-IN-UNITPER,
                   :QCL-PD-UNITPER-PER-YEAR,
                   :QCL-PD-REPOCD, 
                   :QCL-PD-BR-TFR-FG, 
                   :QCL-PD-O3INS-DLPROC,
                   :QCL-PD-INSPREM-DLADJ,
                   :QCL-PD-PLAMT,
                   :QCL-PD-MTD-PRIN,
                   :QCL-PD-MTD-LC,
                   :QCL-PD-MTD-INT,
                   :QCL-PD-MTD-OT,
                   :QCL-PD-MTD-EX,
                   :QCL-PD-BNKRPT-CHAPTER,
                   :QCL-PD-BMO-CON-STATUS, 
                   :QCL-PD-BMO-REC-STATUS, 
                   :QCL-PD-APP-NUMBER,
                   :QCL-PD-GIFTNET, 
                   :QCL-PD-GIFTNET-OUR-PO,
                   :QCL-PD-PROC-DISBURSED, 
                   :QCL-PD-NOODDPY,
                   :QCL-PD-CONPROFILE-1,
                   :QCL-PD-CONPROFILE-2,
                   :QCL-PD-CONPROFILE-3,
                   :QCL-PD-CONPROFILE-4,
                   :QCL-PD-CONPROFILE-5,
                   :QCL-PD-RECPROFILE-1,
                   :QCL-PD-RECPROFILE-2,
                   :QCL-PD-RECPROFILE-3,
                   :QCL-PD-RECPROFILE-4,
                   :QCL-PD-RECPROFILE-5,
                   :QCL-PD-TRW-STATUS, 
                   :QCL-PD-ORIGIN, 
                   :QCL-PD-PO-TRCD, 
                   :QCL-PD-NEXIS-CRSCORE,
                   :QCL-PD-TRW-B-CONSUMER-IND, 
                   :QCL-PD-PARVHELD-PAID-FG, 
                   :QCL-PD-ACCT-FROZEN, 
                   :QCL-PD-TRW-SPECIAL-CODE, 
                   :QCL-PD-CREATION-STATUS, 
                   :QCL-PD-TOTNOLCHG,
                   :QCL-PD-LBOX, 
                   :QCL-PD-EXTRA-PRIN-LFP,
                   :QCL-PD-REMAIN-INT-SINCE-LFP,
                   :QCL-PD-REMAIN-PRIN-SINCE-LFP,
                   :QCL-PD-INTERIM-COUPON, 
                   :QCL-PD-DLCOMM-O3-CMSI,
                   :QCL-PD-DLCOMM-PP-CMSI,
                   :QCL-PD-TRW-J-CONSUMER-IND, 
                   :QCL-PD-ACH, 
                   :QCL-PD-1099-C-FG, 
                   :QCL-PD-TRW-COMPLIANCE-CODE, 
                   :QCL-PD-UNWIND-REASON-CODE, 
                   :QCL-PD-PBI-PBD-FG, 
                   :QCL-PD-OLD-ACCT-FORMAT-FG, 
                   :QCL-PD-APPROVED-BY-USER, 
                   :QCL-PD-APPROVED-BY-CODE, 
                   :QCL-PD-APPROVED-BY-DATE, 
                   :QCL-PD-APPROVED-BY-TIME, 
                   :QCL-PD-LTOUCH-DATE, 
                   :QCL-PD-LTOUCH-TIME,
                   :QCL-PD-HOLD-COLLATERAL, 
                   :QCL-PD-DLPAID, 
                   :QCL-PD-CKNO,
                   :QCL-PD-FREE1, 
                   :QCL-PD-FREE2, 
                   :QCL-PD-FREE-SPACE-FILLER, 
                   :QCL-PD-POSTING-BR,
                   :QCL-PD-OTHBR-FG, 
                   :QCL-PD-APPROVAL-LIMIT,
                   :QCL-PD-APPROVAL-USERID, 
                   :QCL-PD-MIN-CRED-SCORE,
                   :QCL-PD-CRED-SCORE-USERID,  
                   :QCL-PD-FINAMT,
                   :QCL-PD-PROCEEDS-FG, 
                   :QCL-PD-POINTS-FG, 
                   :QCL-PD-OVRDRATE-FG, 
                   :QCL-PD-INS-FG-1, 
                   :QCL-PD-INS-FG-2, 
                   :QCL-PD-INS-FG-3, 
                   :QCL-PD-INS-FG-4, 
                   :QCL-PD-INS-FG-5, 
                   :QCL-PD-INS-FG-6, 
                   :QCL-PD-INS-FG-7, 
                   :QCL-PD-INS-FG-8, 
                   :QCL-PD-INS-FG-9, 
                   :QCL-PD-INS-FG-10, 
                   :QCL-PD-SERCHG-FG, 
                   :QCL-PD-MAINTFEE-FG, 
                   :QCL-PD-EXTCHG-FG, 
                   :QCL-PD-REGPYAMT-FG, 
                   :QCL-PD-FEECD-1, 
                   :QCL-PD-FEECD-2, 
                   :QCL-PD-FEECD-3, 
                   :QCL-PD-FEECD-4, 
                   :QCL-PD-FEECD-5, 
                   :QCL-PD-FEECD-6, 
                   :QCL-PD-FEECD-7, 
                   :QCL-PD-FEECD-8, 
                   :QCL-PD-FEECD-9, 
                   :QCL-PD-FEECD-10, 
                   :QCL-PD-FEECD-11, 
                   :QCL-PD-FEECD-12, 
                   :QCL-PD-FEECD-13, 
                   :QCL-PD-FEECD-14, 
                   :QCL-PD-FEECD-15, 
                   :QCL-PD-FEECD-16, 
                   :QCL-PD-FEECD-17, 
                   :QCL-PD-FEECD-18, 
                   :QCL-PD-FEECD-19, 
                   :QCL-PD-FEECD-20, 
                   :QCL-PD-PRPAID-AHEAD,
                   :QCL-PD-RESTRICT-ADJ,
                   :QCL-PD-DUAL-LOAN-ADJ,
                   :QCL-PD-CAN-LOAN-MAX,
                   :QCL-PD-PERDOWN,
                   :QCL-PD-SELLPR-FG, 
                   :QCL-PD-DOWNPY-FG, 
                   :QCL-PD-PERDOWN-FG, 
                   :QCL-PD-SELL-DOWN-FG, 
                   :QCL-PD-BALLOON-FG, 
                   :QCL-PD-BALLOON-LAST-FG, 
                   :QCL-PD-FEECD-FG-1, 
                   :QCL-PD-FEECD-FG-2, 
                   :QCL-PD-INS-TERM-SW-1, 
                   :QCL-PD-INS-TERM-SW-2, 
                   :QCL-PD-INS-TERM-SW-3, 
                   :QCL-PD-INS-TERM-SW-4, 
                   :QCL-PD-INS-TERM-SW-5, 
                   :QCL-PD-INS-TERM-SW-6, 
                   :QCL-PD-INS-TERM-SW-7, 
                   :QCL-PD-INS-TERM-SW-8, 
                   :QCL-PD-INS-TERM-SW-9, 
                   :QCL-PD-INS-TERM-SW-10, 
                   :QCL-PD-COVERAGE-SW-1, 
                   :QCL-PD-COVERAGE-SW-2, 
                   :QCL-PD-COVERAGE-SW-3, 
                   :QCL-PD-COVERAGE-SW-4, 
                   :QCL-PD-COVERAGE-SW-5, 
                   :QCL-PD-COVERAGE-SW-6, 
                   :QCL-PD-COVERAGE-SW-7, 
                   :QCL-PD-COVERAGE-SW-8, 
                   :QCL-PD-COVERAGE-SW-9, 
                   :QCL-PD-COVERAGE-SW-10, 
                   :QCL-PD-PREMIUM-SW-1, 
                   :QCL-PD-PREMIUM-SW-2, 
                   :QCL-PD-PREMIUM-SW-3, 
                   :QCL-PD-PREMIUM-SW-4, 
                   :QCL-PD-PREMIUM-SW-5, 
                   :QCL-PD-PREMIUM-SW-6, 
                   :QCL-PD-PREMIUM-SW-7, 
                   :QCL-PD-PREMIUM-SW-8, 
                   :QCL-PD-PREMIUM-SW-9, 
                   :QCL-PD-PREMIUM-SW-10, 
                   :QCL-PD-DLDISC-FG, 
                   :QCL-PD-DLRVPRIN-FG-1, 
                   :QCL-PD-DLRVPRIN-FG-2, 
                   :QCL-PD-DLRVPRIN-FG-3, 
                   :QCL-PD-DLRVCHGS-FG-1, 
                   :QCL-PD-DLRVCHGS-FG-2, 
                   :QCL-PD-DLRVCHGS-FG-3, 
                   :QCL-PD-DLPARVHELD-FG, 
                   :QCL-PD-DLPARVPAID-FG, 
                   :QCL-PD-ONE-MTH-NO-DAY-EXT-SW, 
                   :QCL-PD-ASSIGN-RN-ACCTNO, 
                   :QCL-PD-APR-FINAMT,
                   :QCL-PD-DLCOMM-PP-CMSI-FG, 
                   :QCL-PD-DLCOMM-O3-CMSI-FG, 
                   :QCL-PD-FILLER-TO-MATCH-LN, 
                   :QCL-PD2-ID, 
                   :QCL-PD2-BRANCH,
                   :QCL-PD2-BWNAME,
                   :QCL-PD-STATE-WORKER, 
                   :QCL-PD-SERCHG, 
                   :QCL-PD-POINTS, 
                   :QCL-PD-CLASS,
                   :QSPA-ORGST, 
                   :QSPA-SPRCLASS, 
                   :QSPA-SUBCLASS, 
                   :QSPA-LAWCODE, 
                   :QSPA-MFFRMLA, 
                   :QSPA-CAL-CHGABLE-1,
                   :QSPA-CAL-CHGABLE-2
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CL-FIELDS
              PERFORM GET-SPA-FIELDS.

           MOVE STAT---GOOD TO CL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CL1-FILE.
           PERFORM CLOSE-IT.
           IF ( CL1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CL_CONNECT END-EXEC
              EXEC SQL
                   CLOSE CL1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO CL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-CL1-CONNECTION.

           IF CL-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CL_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CL_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CL-CONN-OPEN-SW.


       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       
       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CLEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
