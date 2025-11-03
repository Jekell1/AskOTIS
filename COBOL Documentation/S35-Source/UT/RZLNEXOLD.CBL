       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNEXTR.
       DATE-WRITTEN.    121201.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  DATA WAREHOUSE LOAN EXTRACTION
      *
      * CREATES AN OUTPUT EXTRACT FILE  /USR/EXTRACT/LOAN
      *                           F3-ALL       = LOAN
      *                           BRANCH RANGE = LOAN.(END-BRANCH)
      *                           GROUP        = LOAN.(GROUP)
      * RSZ 3597 INCLUDING CR,LF
      *
      * REV:
      *  011212     REGIONAL ACCEPTANCE, PR 1675
      *  040907 CS  ADD BR-REGION-NUMBER TO OUTPUT-FILE XLN-REGION-NUMBER
      *                                       [PR#76 REGACC]
      *  050429 BAH ADDED LN-ACH, REGACC PR# 102
      *  050526 BAH ADDED ZERO BALANCES FROM DATE, LENDMARK PR# 2539
      *             SKIP PAIDOUTS PRIOR TO THIS DATE
      *  BAH 050601 CHANGED LN-TRW-1ST-30 TO LN-TRW-DOFD-DATE, DATE OF FIRST
      *             DELINQUENCY
      *  BAH 051019 ADDED LN-UNWIND-REASON-CODE, REGACC PR# 105
      *  BAH 051025 REMOVED LN-NOODDPY-PD-JUNK , LN-LEDGERCNT, LN-LPCNT
      *  BAH 051028 ADDED LN-FROZEN-REASON, REGACC PR# 114
      *  BAH 061205 ADDED LN-COUPON-STATEMENT REGACC PR# 158
      *  BAH 100824 ADDED XLN-NETBAL FOR REGACC PR# 271
      *  BAH 110331 ADDED XLN-10DAY-PAYOFF REDEFINE XLN-RCURPRIN FIELD
      *              FOR SOAUTO ONLY, SOAUTO PR# 3762
      *  BAH 150615 EXPANDED XAE-KEY TO 9(9) FROM 9(7), WORLD PR# 1088
      *  151007 BAH ADDED XLN-TRW-B-CONSUMER-IND,XLN-TRW-J-CONSUMER-IND,
      *             XLN-TRW-COMPLIANCE-CODE,  SOAUTO PL#906
      *  BAH 160201 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      *  160519 BAH MOVE RECDEL TO XNO-RECENCY FOR WORLD ONLY PR#1159
      *  161114 BAH ADDED NEW OPTION ON THE SCREEN TO CREATE XNO ON
      *             PAID OUTS Y/N, WORLD PR# 1205
      *  161221 MJD ADDED D2 > D5 AMOUNTS TO END OF EXTRACT 71225 #384
      *  170110 MJD IF 71225 DO NOT DISPLAY DF-D5 FOR BRANCHES 1 THRU 7.
      *             PERFORM DEFERMENT POLICY TEST PRIOR TO CALCULATING D2-D5
      *                                                     71225 #384A
      *  170125 MJD ADDED "MOVE POFF-NETDUE TO DEF-TOTAL-INTDUE" PRIOR TO
      *             CALCULATING  DEFERMENTS                 71225 #384A
      *  170131 MJD CHANGE POFF-NETDUE TO POFF-INTDUE WHEN CALLING DEFERMENT
      *             CALCULATION.                            71225 #384A
      *  170131 MJD CHANGE TO CAL D2-D5 AT SAME TIME AS DF  71225 #384A
      *  170331 MJD REMOVED D2-D5 FROM END OF EXTRACT AND REPLACED WITH 
      *             DF-D9 CALCULATED USING THE SP-ALT-DEFFRMLA.  ONLY DISPLAY 
      *             IF BR_STATE = TX AND BRANCH > 7.  BRANCHES 1-7 ARE PL 
      *             BRANCHES AND SHOULD NOT BE INCLUDED.               71225#386
      *
      * KEC 2017.0403 {PR#05271} HONOR FIANCNE  <A15> [DAVID SINCLAIR]
      *        ADDED XXX-TO-ROLL WHICH IS A CALCULATED FIELD (WI/COLAST)
      *
      * KEC 2017.0522 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED THE USE OF PDTH-WK-MO/DA/YR
      *                       TO BE PDTH-WK-MM/DD/YY
      *
      * BAH 2017.1229 CONVERTED DATES
      *  CS 180529 FIXED LN-PRLNNO TO HAVE 01SEQ TO MATCH A15
      * BAH 2018.0905 LARM30, REMOVE MA
      * BAH 2018.1105 ADDED PL-FROM-DATE SCREEN INPUT, DO NOT INCLUDE P&L
      *               PRIOR TO THIS DATE, ONLY IF LN-DATE-PAID-LAST IS ALSO 
      *               PRIOR TO THIS DATE +3 YEARS, WORLD #1346
      * BAH 2019.0201 TEST-DATE-PAID-LAST WAS NOT GETTING SET IN BATCH, 1346A
      * BAH 2019.0708 XLN-PYMMYY-RECDEL WAS NOT SET CORRECTLY PR#1375
      *               XLN-OLDACCTNO AND XLN-PRLNNO HAD 01 FOR SEQ EVEN IF
      *               ACCOUNT # WAS 0.
      * BAH 20190722 ADDED SUFFIX TO PATH SO CAN MULTIPLE AT SAME TIME, #1381
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220812 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXLN.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXLN.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/LNEXTR Z35 10/18/23-10:33:33 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
      *********************************************************
      *   LIBEX/EXWSXLN: LOAN EXTRACTION FILE PATH WORKERS
      *********************************************************
       01  XLN-PATH.
           03  XLN-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XLN-FILENAME       PIC X(4) VALUE "LOAN".
           03  XLN-EXTENSION.
               05  XLN-PATH-DOT       PIC X.
               05  XLN-PATH-GROUP     PIC X(4).
               05  XLN-PATH-BRNO REDEFINES XLN-PATH-GROUP PIC 9(4).

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LPWSLM.CPY".
      *----------------------------------------------------------------
       01  XLN-KEY       PIC 9(9)     VALUE 0.
       01  WS-A15-ACCTNO.
           03  WS-NEW-NUMBER   PIC 9(6).
           03  WS-NEW-SEQNO    PIC 9(2).
       01  WS-BRACCTNO.
           03  WS-OWNBR    PIC 9(4).
           03  WS-ACCTNO   PIC 9(6).

       01  WS-LSTLPKEY         PIC X(17).
       01  FILLER REDEFINES WS-LSTLPKEY.
           03  WS-LSTLP-BRNO   PIC 9(4).
           03  WS-LSTLP-ACCTNO PIC X(8).
           03  WS-LSTLP-SEQNO  PIC 9(5).
           03  WS-LSTLP-SEQNOX REDEFINES WS-LSTLP-SEQNO PIC X(5).

       01  WS-FIX-LSTLPKEY PIC X(12).
       01  FILLER REDEFINES WS-FIX-LSTLPKEY.
           03  WS-FIX-LSTLP-ACCTNO PIC X(8).
           03  WS-FIX-LSTLP-SEQNO  PIC 9(4).
       

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

       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

       01  RECDEL-MMYY          PIC 9(4).
       01  RECDEL-X REDEFINES RECDEL-MMYY.
           03  RECDEL-MM        PIC 99.
           03  RECDEL-YY        PIC 99.
       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  AGE-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  BEG-CLASS              PIC 99.
           03  END-CLASS              PIC 99.
           03  PL                     PIC X.
           03  PLTYPE                 PIC X.
           03  PLACTIVE               PIC X.
           03  ZBAL                   PIC X.
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  ZBAL-DATE              PIC 9(8).
           03  CREATE-NOTICE          PIC X.
           03  PL-FROM-DATE           PIC 9(8).

       01  VERSION-CONTROL     PIC X    VALUE "B".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  TEST-DATE-PAID-LAST PIC 9(8) VALUE 0.

      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------

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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-TITLE          PIC X(20)    VALUE
           "LOAN FILE EXTRACTION".
           03  FILLER           PIC X(6)     VALUE " FOR: ".
           03  H-AGEDATE        PIC X(10).

      *-----------------------------------------------------------------

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  HOLD-TODAY-DATE      PIC 9(8)     COMP.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "N".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-COUNT           PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(111).
           03  RANGE-SELNUM    REDEFINES RANGE-SELECTION.
               05  RANGE-DATE    PIC 9999/99/99.
               05  FILLER        PIC X(101).

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".

       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LNEXTR_DEF.CPY".
       COPY "LIBSP/LNEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LNEXTR_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XLN-FILE.
      *-----------------------------------------------------------------
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
      *-----------------------------------------------------------------
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XLN-FILE.
      *-----------------------------------------------------------------
       COPY "LIBGB/DECLRP2.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNEXTR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      * SET THE 3+ YEARS FOR TEST-DATE-PAID-LAST
           MOVE 0 TO TEST-DATE-PAID-LAST.
           IF (PL = "I" OR "O") AND (PL-FROM-DATE NOT = 0)
              MOVE PL-FROM-DATE TO NDTE-DATE
              MOVE 3 TO NDTE-HOLD
              PERFORM INCREMENT-YEARS
              MOVE NDTE-DATE TO TEST-DATE-PAID-LAST.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
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

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              GB-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM OPEN-LN3-FILE.

      * THIS DOES WORK, JUST HAVE TO CLEAR THE MESS BEFORE EXITING
      *    IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.


           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           PERFORM NPR-TRAP-INTERRUPT.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  POCD - **  IS A DELETED CONTRACT
      *  KEVIN SAYS DON'T SKIP DELETED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( LN-POCD-NONLOAN )
      *       GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS
              GO TO NEXT-LN.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.
           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.
           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.
           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.
           IF LN-PLDATE NOT = 0
              IF PLTYPE NOT = "A"
                 IF LN-PLTYPE NOT = PLTYPE
                    GO TO NEXT-LN.

      * TEST P&L FROM DATE, IF LN-PLDATE IS PRIOR TO THIS DATE, SKIP
      * BUT ONLY IF DATE OF LAST PAYMENT IS OLDER THAN +3 YEARS FROM
      * PL-FROM-DATE
      * EXAMPLE: IF PL-FROM-DATE IS 01/01/2010, SKIP CHARGE OFFS FROM 2009
      *          AND PRIOR, AS LONG AS THE DATE PAID LAST IS OLDER THAN
      *          01/01/2013
           IF LN-PLDATE NOT = 0
              IF LN-PLDATE < PL-FROM-DATE
      * NEVER MADE A PAYMENT
                 IF LN-DATE-PAID-LAST = 0
                    GO TO NEXT-LN
                 ELSE
                    IF LN-DATE-PAID-LAST < TEST-DATE-PAID-LAST
                       GO TO NEXT-LN.

           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0 OR
                  LN-INTBAL NOT = 0 OR
                   LN-LCBAL NOT = 0 OR
                    LN-OTHBAL NOT = 0
                      GO TO NEXT-LN.
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                       GO TO NEXT-LN.

      * TEST ZERO BALANCES FROM DATE, IF PRIOR TO THIS DATE, SKIP
      * IF EQUAL OR GREATER THAN THIS DATE, INCLUDE
           IF LN-POFFDATE NOT = 0
              IF ZBAL-DATE NOT = JULIAN-BEG
                 IF LN-POFFDATE < ZBAL-DATE
                    GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET STATE-PARAMETER-RECORD:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      * DO NOT USE A HOLD-SP1-KEY, MUST RE-READ BECAUSE OF LN-LC-CODE
      * IN THE LATE CHARGE ROUTINES, WIPES OUT IMPORTANT SPR FIELDS !
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-LN.

           IF CREATE-NOTICE = "N" AND LN-CURBAL = 0
              PERFORM CLEAR-NO-FILE
           ELSE
              MOVE LN-ACCTNO       TO CRNOR2-ACCTNO
              MOVE HOLD-TODAY-DATE TO CRNOR2-SYSDATE
              MOVE "CL/CRNOR2"     TO FORM-NAM
              CALL "CRNOR2" USING FORM-PATH CRNOR2-LINK NO-REC
                                    POFF-WORKERS LN-REC
              CANCEL "CRNOR2".

      * CANT TRUST PDTH-DATE
           MOVE NO-PDTH-DATE TO PDTH-DATE-WORK.
           MOVE PDTH-WK-MM TO PDTH-MM.
           IF LN-UNITPER-CD-DAYS
              MOVE PDTH-WK-DD TO PDTH-YY
           ELSE
              MOVE PDTH-WK-YY TO PDTH-YY.

      *-----------------------------------------------------------------
       FILL-WORK-FLDS.
           MOVE SPACES TO XLN-REC.

           MOVE LN-OWNBR                    TO XLN-OWNBR WS-OWNBR.
           MOVE LN-ACCTNO                   TO XLN-ACCTNO WS-ACCTNO.
           MOVE 01                          TO XLN-SEQNO.
           MOVE WS-BRACCTNO                 TO XLN-BRACCTNO.
           MOVE BR-REGION-NUMBER            TO XLN-REGION-NUMBER.
           MOVE LN-CLASS                    TO XLN-CLASS.
           MOVE LN-SUBCLASS                 TO XLN-SUBCLASS.
           MOVE LN-LAWCODE                  TO XLN-LAWCODE.
           MOVE LN-SSNO(1)                  TO XLN-SSNO1.
           MOVE LN-MAKERCD(1)               TO XLN-MAKERCD1.
           MOVE LN-SSNO(2)                  TO XLN-SSNO2.
           MOVE LN-MAKERCD(2)               TO XLN-MAKERCD2.
           MOVE LN-SSNO(3)                  TO XLN-SSNO3.
           MOVE LN-MAKERCD(3)               TO XLN-MAKERCD3.
           MOVE LN-SSNO(4)                  TO XLN-SSNO4.
           MOVE LN-MAKERCD(4)               TO XLN-MAKERCD4.
           MOVE LN-ORGST                    TO XLN-ORGST.
           MOVE LN-ORGBR                    TO XLN-ORGBR.
           MOVE LN-OWNST                    TO XLN-OWNST.
           MOVE LN-SPRCLASS                 TO XLN-SPRCLASS.
*FIX*      MOVE SPACES                      TO XLN-MORELN.
           MOVE LN-CONVERCD                 TO XLN-CONVERCD.
           MOVE LN-MIDSTREAM                TO XLN-MIDSTREAM.
           MOVE LN-PURCD                    TO XLN-PURCD.
           MOVE LN-SRCD                     TO XLN-SRCD.
           MOVE LN-COLLCD                   TO XLN-COLLCD.
           MOVE LN-COLLID                   TO XLN-COLLID.
           MOVE LN-POCD                     TO XLN-POCD.
           MOVE LN-PLCD                     TO XLN-PLCD.
           MOVE LN-PLTYPE                   TO XLN-PLTYPE.
           MOVE LN-CONFIDENTCD              TO XLN-CONFIDENTCD.
           MOVE LN-ALLOTCD                  TO XLN-ALLOTCD.
           MOVE LN-RECASTCD                 TO XLN-RECASTCD.
           MOVE LN-SKIP-TRW                 TO XLN-SKIP-TRW.
           MOVE LN-STATUSFG                 TO XLN-STATUSFG.
           MOVE LN-SMALLOAN                 TO XLN-SMALLOAN.
           MOVE LN-SALEFIN                  TO XLN-SALEFIN.
           MOVE LN-SERVACCT                 TO XLN-SERVACCT.
           MOVE LN-LOANTYPE                 TO XLN-LOANTYPE.
           MOVE LN-ADDON-FG                 TO XLN-ADDON-FG.
           MOVE LN-PAY-SCHLD-FG             TO XLN-PAY-SCHLD-FG.
           MOVE LN-LC-CODE                  TO XLN-LC-CODE.
           MOVE LN-CAN-LOAN-NO              TO XLN-CAN-LOAN-NO.
           MOVE LN-STATFLAGS                TO XLN-STATFLAGS.

           MOVE LN-ENTDATE                  TO XLN-ENTDATE.
           MOVE LN-LOANDATE                 TO XLN-LOANDATE.
           MOVE LN-PURDATE                  TO XLN-PURDATE.
           MOVE LN-INTDATE                  TO XLN-INTDATE.
           MOVE LN-CASHDATE                 TO XLN-CASHDATE.
           MOVE LN-FILEDATE                 TO XLN-FILEDATE.
           MOVE LN-ORIG-1STPYDATE           TO XLN-ORIG-1STPAY.
           MOVE LN-1STPYDATE                TO XLN-1STPYDATE.
           MOVE LN-JDDATE                   TO XLN-JDDATE.
           MOVE LN-PLDATE                   TO XLN-PLDATE.
           MOVE LN-ACCELDATE                TO XLN-ACCELDATE.
           MOVE LN-REDUDATE                 TO XLN-REDUDATE.
           MOVE LN-POFFDATE                 TO XLN-POFFDATE.

           MOVE LN-LEGALDATE                TO XLN-LEGALDATE.
           MOVE LN-BNKRPTDATE               TO XLN-BNKRPTDATE.
           MOVE LN-DEBTAIDATE               TO XLN-DEBTAIDATE.
           MOVE LN-REPODATE                 TO XLN-REPODATE.
           MOVE LN-REDEEMDATE               TO XLN-REDEEMDATE.
           MOVE LN-SOLDDATE                 TO XLN-SOLDDATE.
           MOVE LN-INS-CONFIRM-DATE         TO XLN-INS-CONFIRMATION.

           MOVE LN-APRATE                   TO XLN-APRATE.
           MOVE LN-EFFRATE                  TO XLN-EFFRATE.
           MOVE LN-SMPRATE                  TO XLN-SMPRATE.
           MOVE LN-YLDRATE                  TO XLN-YLDRATE.
           MOVE LN-OVRDRATE                 TO XLN-OVRDRATE.
           MOVE LN-ENDORATE                 TO XLN-ENDORATE.
           MOVE LN-JDRATE                   TO XLN-JDRATE.
           MOVE LN-ACCRATE                  TO XLN-ACCRATE.
           MOVE LN-REDURATE                 TO XLN-REDURATE.
           MOVE LN-SERVPER                  TO XLN-SERVPER.
           MOVE LN-SELLPR                   TO XLN-SELLPR.
           MOVE LN-DOWNPY                   TO XLN-DOWNPY.
           MOVE LN-TRADEIN                  TO XLN-TRADEIN.
           MOVE LN-LNAMT                    TO XLN-LNAMT.
           MOVE LN-FINCHG                   TO XLN-FINCHG.
           MOVE LN-INTCHG                   TO XLN-INTCHG.
           MOVE LN-SERCHG                   TO XLN-SERCHG.
           MOVE LN-EXTCHG                   TO XLN-EXTCHG.
           MOVE LN-MAINTFEE                 TO XLN-MAINTFEE.
           MOVE LN-POINTS                   TO XLN-POINTS.
           MOVE LN-PROCEEDS                 TO XLN-PROCEEDS.
           INSPECT XLN-PROCEEDS REPLACING LEADING "+" BY " ".
           MOVE LN-ORGTERM                  TO XLN-ORGTERM.
           MOVE LN-1STPYAMT                 TO XLN-1STPYAMT.
           MOVE LN-NOREGPY                  TO XLN-NOREGPY.
           MOVE LN-REGPYAMT                 TO XLN-REGPYAMT.
           MOVE LN-LASTPYAMT                TO XLN-LASTPYAMT.
           INSPECT XLN-LASTPYAMT REPLACING LEADING "+" BY " ".
           MOVE LN-PROCASH                  TO XLN-PROCASH.
           INSPECT XLN-PROCASH REPLACING LEADING "+" BY " ".
           MOVE LN-TILLNO                   TO XLN-TILLNO.
           MOVE LN-STATE-FLAG               TO XLN-STATE-FLAG.
           MOVE LN-STATE-WORKER             TO XLN-STATE-WORKER.
           INSPECT XLN-STATE-WORKER REPLACING LEADING "+" BY " ".
           MOVE LN-STATE-WORKER2            TO XLN-STATE-WORKER2.
           INSPECT XLN-STATE-WORKER2 REPLACING LEADING "+" BY " ".
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE LN-FEECODE(SUB)          TO XLN-FEECODE(SUB)
              MOVE LN-FEEAMT(SUB)           TO XLN-FEEAMT(SUB)
              MOVE LN-FEEOURS(SUB)          TO XLN-FEEOURS(SUB)
              MOVE LN-FEEFINCHG(SUB)        TO XLN-FEEFINCHG(SUB)
           END-PERFORM.
           MOVE LN-INSURED                  TO XLN-INSURED.
           MOVE LN-INSTYPES                 TO XLN-INSTYPES.
           MOVE LN-INSELIGIBLE              TO XLN-INSELIGIBLE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              MOVE LN-INSCOMP(SUB)          TO XLN-INSCOMP(SUB)
              MOVE LN-INSOURS(SUB)          TO XLN-INSOURS(SUB)
              MOVE LN-INSEFF-DATE(SUB)      TO XLN-INSEFF(SUB)
              MOVE LN-INSEXP-DATE(SUB)      TO XLN-INSEXP(SUB)
              MOVE LN-INSPREM(SUB)          TO XLN-INSPREM(SUB)
              MOVE LN-INSCOVR(SUB)          TO XLN-INSCOVR(SUB)
              MOVE LN-INSCOMM(SUB)          TO XLN-INSCOMM(SUB)
              MOVE LN-INSANTICERN(SUB)      TO XLN-INSANTICERN(SUB)
              INSPECT XLN-INSANTICERN(SUB) REPLACING LEADING "+" BY " "
              MOVE LN-INSANTICADJ(SUB)      TO XLN-INSANTICADJ(SUB)
              INSPECT XLN-INSANTICADJ(SUB) REPLACING LEADING "+" BY " "
              MOVE LN-INSUFERN(SUB)         TO XLN-INSUFERN(SUB)
              INSPECT XLN-INSUFERN(SUB) REPLACING LEADING "+" BY " "
              MOVE LN-INSACCERN(SUB)        TO XLN-INSACCERN(SUB)
              INSPECT XLN-INSACCERN(SUB) REPLACING LEADING "+" BY " "
              MOVE LN-INSEARN-ORDER(SUB)    TO XLN-INSEARN-ORDER(SUB)
              MOVE LN-INSFINCHG(SUB)        TO XLN-INSFINCHG(SUB)
           END-PERFORM.

           MOVE LN-PY-YYYYMM-RECDEL         TO DATE-YYYYMM.
           MOVE DATE-YYYYMM-YY TO RECDEL-YY.
           MOVE DATE-YYYYMM-MM TO RECDEL-MM.
           MOVE RECDEL-MMYY                 TO XLN-PYMMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL          TO XLN-MTD-PAYS-RECDEL.
           INSPECT XLN-MTD-PAYS-RECDEL REPLACING LEADING "+" BY " ".
*FIX*      MOVE SPACES                      TO XLN-GIFTNET-EXTR.
           MOVE LN-SHIP-SAME-FG             TO XLN-SHIP-SAME-FG.
           MOVE LN-AD-D-INS-COVR            TO XLN-AD-D-INS-COVR.
           MOVE LN-TOT-PARTIAL-CHGOFF       TO XLN-TOT-PARTIAL-CHGOFF.
           INSPECT XLN-TOT-PARTIAL-CHGOFF REPLACING LEADING "+" BY " ".
           MOVE LN-LPO                      TO XLN-LPO.
           MOVE LN-REP-CD                   TO XLN-REP-CD.
           MOVE LN-PROCESS-CD               TO XLN-PROCESS-CD.

           MOVE LN-PRIOR-RN-FOR10-DATE      TO XLN-PRIOR-RN-FOR10.
           MOVE LN-LAST-RN-FOR10-DATE       TO XLN-LAST-RN-FOR10.

           MOVE LN-WCBC-CREDITLIM           TO XLN-WCBC-CREDITLIM.
           INSPECT XLN-WCBC-CREDITLIM REPLACING LEADING "+" BY " ".
           MOVE LN-FOLLOWUP-CD              TO XLN-FOLLOWUP-CD.
           MOVE LN-DLNO                     TO XLN-DLNO.
           MOVE LN-DLPOOL                   TO XLN-DLPOOL.
           MOVE LN-DLRECOURSE               TO XLN-DLRECOURSE.
           MOVE LN-DLPARTRCAMT              TO XLN-DLPARTRCAMT.
           MOVE LN-ORIGPURAMT               TO XLN-ORIGPURAMT.
           MOVE LN-PURBAL                   TO XLN-PURBAL.
           INSPECT XLN-PURBAL REPLACING LEADING "+" BY " ".
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE LN-DLRVPRIN(SUB)         TO XLN-DLRVPRIN(SUB)
              MOVE LN-DLRVCHGS(SUB)         TO XLN-DLRVCHGS(SUB)
           END-PERFORM.
           MOVE LN-DLPARVHELD               TO XLN-DLPARVHELD.
           MOVE LN-DLPARVPAID               TO XLN-DLPARVPAID.
           MOVE LN-DLDISC                   TO XLN-DLDISC.

           MOVE LN-LCPDTH-DATE              TO XLN-LCPDTH.
           MOVE LN-INTPDTH-DATE             TO XLN-INTPDTH.

           MOVE LN-CURBAL                   TO XLN-CURBAL.
           INSPECT XLN-CURBAL REPLACING LEADING "+" BY " ".
           MOVE LN-INTBAL                   TO XLN-INTBAL.
           INSPECT XLN-INTBAL REPLACING LEADING "+" BY " ".
           MOVE LN-LCBAL                    TO XLN-LCBAL.
           INSPECT XLN-LCBAL REPLACING LEADING "+" BY " ".
           MOVE LN-OTHBAL                   TO XLN-OTHBAL.
           INSPECT XLN-OTHBAL REPLACING LEADING "+" BY " ".
           MOVE LN-OT2BAL                   TO XLN-OT2BAL.
           INSPECT XLN-OT2BAL REPLACING LEADING "+" BY " ".
           MOVE LN-LCPAID                   TO XLN-LCPAID.
           INSPECT XLN-LCPAID REPLACING LEADING "+" BY " ".
           MOVE LN-LCPARTIALS               TO XLN-LCPARTIALS.
           INSPECT XLN-LCPARTIALS REPLACING LEADING "+" BY " ".
           MOVE LN-STMT-COUNTER             TO XLN-STMT-COUNTER.
           MOVE LN-MONTHEND-BAL             TO XLN-MONTHEND-BAL.
           MOVE LN-ANTICTERM                TO XLN-ANTICTERM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE LN-ANTICERN(SUB)         TO XLN-ANTICERN(SUB)
              INSPECT XLN-ANTICERN(SUB) REPLACING LEADING "+" BY " "
              MOVE LN-ANTICADJ(SUB)         TO XLN-ANTICADJ(SUB)
              INSPECT XLN-ANTICADJ(SUB) REPLACING LEADING "+" BY " "
              MOVE LN-UFERN(SUB)            TO XLN-UFERN(SUB)
              INSPECT XLN-UFERN(SUB) REPLACING LEADING "+" BY " "
              MOVE LN-ACCERN(SUB)           TO XLN-ACCERN(SUB)
              INSPECT XLN-ACCERN(SUB) REPLACING LEADING "+" BY " "
              MOVE LN-EARN-ORDER(SUB)       TO XLN-EARN-ORDER(SUB)
           END-PERFORM.
           MOVE LN-LMO-PRIOR-INTEREST       TO XLN-LMO-PRIOR-INTEREST.
           INSPECT XLN-LMO-PRIOR-INTEREST REPLACING LEADING "+" BY " ".
           MOVE LN-PRIOR-UNEARNED           TO XLN-PRIOR-UNEARNED.
           INSPECT XLN-PRIOR-UNEARNED REPLACING LEADING "+" BY " ".
           MOVE LN-ERNDELQ-STOP             TO XLN-ERNDELQ-STOP.
           MOVE LN-NONACRERN                TO XLN-NONACRERN.
           INSPECT XLN-NONACRERN REPLACING LEADING "+" BY " ".
           MOVE LN-PRIOR-NONACRERN          TO XLN-PRIOR-NONACRERN.
           INSPECT XLN-PRIOR-NONACRERN REPLACING LEADING "+" BY " ".

           MOVE LN-ERN-IB-PDTH-DATE         TO XLN-ERN-IB-PDTH.

           MOVE LN-ERN-IB-PRINBAL           TO XLN-ERN-IB-PRINBAL.
           INSPECT XLN-ERN-IB-PRINBAL REPLACING LEADING "+" BY " ".
           MOVE LN-ERN-IB-INTOWE            TO XLN-ERN-IB-INTOWE.
           INSPECT XLN-ERN-IB-INTOWE REPLACING LEADING "+" BY " ".
           MOVE LN-TOTERN                   TO XLN-TOTERN.
           INSPECT XLN-TOTERN REPLACING LEADING "+" BY " ".
           MOVE LN-TOTOTH                   TO XLN-TOTOTH.
           INSPECT XLN-TOTOTH REPLACING LEADING "+" BY " ".
           MOVE LN-TOTINT                   TO XLN-TOTINT.
           INSPECT XLN-TOTINT REPLACING LEADING "+" BY " ".
           MOVE LN-TOTLCHG                  TO XLN-TOTLCHG.
           INSPECT XLN-TOTLCHG REPLACING LEADING "+" BY " ".
           MOVE LN-TOTDEF                   TO XLN-TOTDEF.
           INSPECT XLN-TOTDEF REPLACING LEADING "+" BY " ".
           MOVE LN-TOTSERACCT               TO XLN-TOTSERACCT.
           INSPECT XLN-TOTSERACCT REPLACING LEADING "+" BY " ".
           MOVE LN-LCWAIVERS                TO XLN-LCWAIVERS.
           INSPECT XLN-LCWAIVERS REPLACING LEADING "+" BY " ".
           MOVE LN-INTWAIVERS               TO XLN-INTWAIVERS.
           INSPECT XLN-INTWAIVERS REPLACING LEADING "+" BY " ".
           MOVE LN-TOTPAYMNTD               TO XLN-TOTPAYMNTD.
           INSPECT XLN-TOTPAYMNTD REPLACING LEADING "+" BY " ".
           MOVE LN-TOTEXCPAYMNTD            TO XLN-TOTEXCPAYMNTD.
           INSPECT XLN-TOTEXCPAYMNTD REPLACING LEADING "+" BY " ".
           MOVE LN-TOTNODEF                 TO XLN-TOTNODEF.
           INSPECT XLN-TOTNODEF REPLACING LEADING "+" BY " ".
           MOVE LN-YTDNODEF                 TO XLN-YTDNODEF.
           INSPECT XLN-YTDNODEF REPLACING LEADING "+" BY " ".
           MOVE LN-TOTNOALDEL               TO XLN-TOTNOALDEL.
           INSPECT XLN-TOTNOALDEL REPLACING LEADING "+" BY " ".
           MOVE LN-YTDINTPD                 TO XLN-YTDINTPD.
           INSPECT XLN-YTDINTPD REPLACING LEADING "+" BY " ".
           MOVE LN-LYRINTPD                 TO XLN-LYRINTPD.
           INSPECT XLN-LYRINTPD REPLACING LEADING "+" BY " ".
           MOVE LN-TOTINTPD                 TO XLN-TOTINTPD.
           INSPECT XLN-TOTINTPD REPLACING LEADING "+" BY " ".
           MOVE LN-RECASTINT                TO XLN-RECASTINT.
           MOVE LN-RECASTPRIN               TO XLN-RECASTPRIN.

           MOVE LN-TRW-DOFD-DATE            TO XLN-TRW-DOFD.

           MOVE LN-CL-REBATE                TO XLN-CL-REBATE.
           MOVE LN-AH-REBATE                TO XLN-AH-REBATE.
           MOVE LN-PP-REBATE                TO XLN-PP-REBATE.
           MOVE LN-INT-REBATE               TO XLN-INT-REBATE.
           MOVE LN-SC-REBATE                TO XLN-SC-REBATE.
           MOVE LN-MF-REBATE                TO XLN-MF-REBATE.
           MOVE LN-DF-REBATE                TO XLN-DF-REBATE.
           MOVE LN-EX-REBATE                TO XLN-EX-REBATE.
           MOVE LN-O1-REBATE                TO XLN-O1-REBATE.
           MOVE LN-O2-REBATE                TO XLN-O2-REBATE.
           MOVE LN-O3-REBATE                TO XLN-O3-REBATE.
           MOVE LN-O4-REBATE                TO XLN-O4-REBATE.
           MOVE LN-O5-REBATE                TO XLN-O5-REBATE.
           MOVE LN-COLLECTOR                TO XLN-COLLECTOR.
           MOVE LN-ACTIONCD                 TO XLN-ACTIONCD.
           MOVE LN-ACTIONCD2                TO XLN-ACTIONCD2.
           MOVE LN-ATCODE                   TO XLN-ATCODE.
           MOVE LN-RECODE                   TO XLN-RECODE.
           MOVE LN-PROMID                   TO XLN-PROMID.

           MOVE LN-PROMDATE                 TO XLN-PROMDATE.

           MOVE LN-PROMAMT                  TO XLN-PROMAMT.

           MOVE LN-DATE-PAID-LAST           TO XLN-DATE-PAID-LAST.
           MOVE LN-AMT-PAID-LAST            TO XLN-AMT-PAID-LAST.
           MOVE LN-LASTCFORMNO              TO XLN-LASTCFORMNO.

           MOVE LN-LASTCFORMDATE            TO XLN-LASTCFORMDATE.

           MOVE LN-DELCNT                   TO XLN-DELCNT.
           MOVE LN-DELCNT2                  TO XLN-DELCNT2.
           MOVE LN-CREDITLIM                TO XLN-CREDITLIM.
           MOVE LN-CRSCORE                  TO XLN-CRSCORE.
           INSPECT XLN-CRSCORE REPLACING LEADING "+" BY " ".
           MOVE LN-LASTSFORMNO              TO XLN-LASTSFORMNO.

           MOVE LN-LASTSFORMDATE            TO XLN-LASTSFORMDATE.

           MOVE LN-SOLCNT                   TO XLN-SOLCNT.
           MOVE LN-MESSAGE                  TO XLN-MESSAGE.

           MOVE LN-PROMDATE2                TO XLN-PROMDATE2.

      *A30 DOES NOT HAVE THE 01, NEED OUTPUT TO MATCH A15 SO
      *ADD BACK THE 01 SEQ
      *    MOVE LN-OLDACCTNO                TO XLN-OLDACCTNO.
      *    MOVE LN-PRLNNO                   TO XLN-PRLNNO.
 
           MOVE LN-OLDACCTNO                TO WS-NEW-NUMBER.
           IF WS-NEW-NUMBER = 0
              MOVE 0                        TO WS-NEW-SEQNO
           ELSE
              MOVE 1                        TO WS-NEW-SEQNO.
           MOVE WS-A15-ACCTNO               TO XLN-OLDACCTNO.

           MOVE LN-PRLNNO                   TO WS-NEW-NUMBER.
           IF WS-NEW-NUMBER = 0
              MOVE 0                        TO WS-NEW-SEQNO
           ELSE
              MOVE 1                        TO WS-NEW-SEQNO.
           MOVE WS-A15-ACCTNO               TO XLN-PRLNNO.     

           MOVE LN-PRBAL                    TO XLN-PRBAL.
           INSPECT XLN-PRBAL REPLACING LEADING "+" BY " ".
           MOVE LN-PRINTDUE                 TO XLN-PRINTDUE.
           INSPECT XLN-PRINTDUE REPLACING LEADING "+" BY " ".
           MOVE LN-PRFINCHGRB               TO XLN-PRFINCHGRB.
           INSPECT XLN-PRFINCHGRB REPLACING LEADING "+" BY " ".
           MOVE LN-PRINSRB                  TO XLN-PRINSRB.
           INSPECT XLN-PRINSRB REPLACING LEADING "+" BY " ".
           INSPECT XLN-PRINSRB REPLACING LEADING "+" BY " ".
           MOVE LN-PRPOCD                   TO XLN-PRPOCD.

           MOVE LN-PRPODATE                 TO XLN-PRPODATE.

           MOVE LN-PRCONPROFILE             TO XLN-PRCONPROFILE.
           MOVE LN-PRDELINQAMT              TO XLN-PRDELINQAMT.
           INSPECT XLN-PRDELINQAMT REPLACING LEADING "+" BY " ".
           MOVE LN-SUM-ORIGPURAMT           TO XLN-SUM-ORIGPURAMT.
           MOVE LN-SUM-LNAMT                TO XLN-SUM-LNAMT.
           MOVE LN-SUM-FINCHG               TO XLN-SUM-FINCHG.
           MOVE LN-SUM-INTCHG               TO XLN-SUM-INTCHG.
           MOVE LN-SUM-ORGTERM              TO XLN-SUM-ORGTERM.
           MOVE LN-SUM-REGPYAMT             TO XLN-SUM-REGPYAMT.
           MOVE LN-SUM-EXTCHG               TO XLN-SUM-EXTCHG.
           MOVE LN-SUM-SERCHG               TO XLN-SUM-SERCHG.
           MOVE LN-SUM-MAINTFEE             TO XLN-SUM-MAINTFEE.
           MOVE LN-UNITPER-CD               TO XLN-UNITPER-CD.
           MOVE LN-UNITPER-FREQ             TO XLN-UNITPER-FREQ.
           MOVE LN-DAYS-IN-UNITPER          TO XLN-DAYS-IN-UNITPER.
           MOVE LN-UNITPER-PER-YEAR         TO XLN-UNITPER-PER-YEAR.
           MOVE LN-REPOCD                   TO XLN-REPOCD.
           MOVE LN-BR-TFR-FG                TO XLN-BR-TFR-FG.
           MOVE LN-O3INS-DLPROC             TO XLN-O3INS-DLPROC.
           MOVE LN-INSPREM-DLADJ            TO XLN-INSPREM-DLADJ.
           INSPECT XLN-INSPREM-DLADJ REPLACING LEADING "+" BY " ".
           MOVE LN-PRLNAMT                  TO XLN-PRLNAMT.
           MOVE LN-PRPDUE-DAYS              TO XLN-PRPDUE-DAYS.
           INSPECT XLN-PRPDUE-DAYS REPLACING LEADING "+" BY " ".
           MOVE LN-PLAMT                    TO XLN-PLAMT.
           MOVE LN-MTD-PRIN                 TO XLN-MTD-PRIN.
           INSPECT XLN-MTD-PRIN REPLACING LEADING "+" BY " ".
           MOVE LN-MTD-LC                   TO XLN-MTD-LC.
           INSPECT XLN-MTD-LC REPLACING LEADING "+" BY " ".
           MOVE LN-MTD-INT                  TO XLN-MTD-INT.
           INSPECT XLN-MTD-INT REPLACING LEADING "+" BY " ".
           MOVE LN-MTD-OT                   TO XLN-MTD-OT.
           INSPECT XLN-MTD-OT REPLACING LEADING "+" BY " ".
           MOVE LN-MTD-EX                   TO XLN-MTD-EX.
           INSPECT XLN-MTD-EX REPLACING LEADING "+" BY " ".
           MOVE LN-BNKRPT-CHAPTER           TO XLN-BNKRPT-CHAPTER.
           MOVE LN-BMO-CON-STATUS           TO XLN-BMO-CON-STATUS.
           MOVE LN-BMO-REC-STATUS           TO XLN-BMO-REC-STATUS.
           MOVE LN-APP-NUMBER               TO XLN-APPNO.
           MOVE LN-GIFTNET                  TO XLN-GIFTNET.
           MOVE LN-GIFTNET-OUR-PO           TO XLN-GIFTNET-OUR-PO.
           MOVE LN-PROC-DISBURSED           TO XLN-PROC-DISBURSED.
           MOVE LN-COUPON-STATEMENT         TO XLN-COUPON-STATEMENT.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE LN-CONPROFILE(SUB)       TO XLN-CONPROFILE(SUB)
              MOVE LN-RECPROFILE(SUB)       TO XLN-RECPROFILE(SUB)
           END-PERFORM.
           MOVE LN-TRW-STATUS               TO XLN-TRW-STATUS.
           MOVE LN-ORIGIN                   TO XLN-ORIGIN.

           MOVE LN-FOLLOWUP-DATE            TO XLN-FOLLOWUP-DATE.

           MOVE LN-PO-TRCD                  TO XLN-PO-TRCD.
           MOVE LN-PRDLDISC-UNEARN          TO XLN-PRDLDISC-UNEARN.
           INSPECT XLN-PRDLDISC-UNEARN REPLACING LEADING "+" BY " ".
           MOVE LN-YTDNOALDEL               TO XLN-YTDNOALDEL.
           INSPECT XLN-YTDNOALDEL REPLACING LEADING "+" BY " ".
      *    MOVE LN-REVOLVING                TO XLN-REVOLVING.

           MOVE 0                           TO XLN-PREV-STMT-DATE
                                               XLN-LAST-STMT-DATE
                                               XLN-ZERO-BAL-CNT.
           MOVE SPACES                      TO XLN-ACTIVE-FG.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
*FIX*         MOVE 0                        TO XLN-AGEING-AMT(SUB)
              INSPECT XLN-AGEING-AMT(SUB) REPLACING LEADING "+" BY " "
           END-PERFORM.

      * WAS LN-CYCLE-DATE IN A15 AS A REDEFINE
           MOVE LN-BNKRPT-DISMISS-DATE      TO XLN-CYCLE-DATE.

      * REVOLVING FIELDS MOVED TO TRAILER
*FIX*      MOVE SPACES     TO XLN-CHARGE-INTEREST-FG.
*FIX*      MOVE SPACES     TO XLN-CHARGE-LATECHG-FG.
*FIX*      MOVE SPACES     TO XLN-CHARGE-MEMBERSHIP-FG.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
*FIX*          MOVE 0      TO XLN-INDEX-ADJUST-FACTOR(SUB)
               INSPECT XLN-INDEX-ADJUST-FACTOR(SUB)
                           REPLACING LEADING "+" BY " "
           END-PERFORM.

      * FIX ALL THESE REVOLVING FIELDS
*FIX*      MOVE 0               TO XLN-OTHBAL-CL.
           INSPECT XLN-OTHBAL-CL REPLACING LEADING "+" BY " ".
           MOVE 0                TO XLN-OTHBAL-AH.
           INSPECT XLN-OTHBAL-AH REPLACING LEADING "+" BY " ".
           MOVE 0                TO XLN-OTHBAL-PP.
           INSPECT XLN-OTHBAL-PP REPLACING LEADING "+" BY " ".
           MOVE 0                TO XLN-OTHBAL-O1.
           INSPECT XLN-OTHBAL-O1 REPLACING LEADING "+" BY " ".
           MOVE 0                TO XLN-OTHBAL-MEMBERSHIP.
           INSPECT XLN-OTHBAL-MEMBERSHIP REPLACING LEADING "+" BY " ".
           MOVE 0                TO XLN-OTHBAL-NSF.
           INSPECT XLN-OTHBAL-NSF REPLACING LEADING "+" BY " ".
           MOVE 0             TO XLN-OTHBAL-MISC1.
           INSPECT XLN-OTHBAL-MISC1 REPLACING LEADING "+" BY " ".
           MOVE 0             TO XLN-OTHBAL-MISC2.
           INSPECT XLN-OTHBAL-MISC2 REPLACING LEADING "+" BY " ".
           MOVE 0             TO XLN-OTHBAL-MISC3.
           INSPECT XLN-OTHBAL-MISC3 REPLACING LEADING "+" BY " ".
           MOVE 0             TO XLN-OTHBAL-MISC4.
           INSPECT XLN-OTHBAL-MISC4 REPLACING LEADING "+" BY " ".
           MOVE 0             TO XLN-OTHBAL-MISC5.
           INSPECT XLN-OTHBAL-MISC5 REPLACING LEADING "+" BY " ".
           MOVE 0             TO XLN-OTHBAL-MISC6.
           INSPECT XLN-OTHBAL-MISC6 REPLACING LEADING "+" BY " ".

      * FIX ALL THESE RVOLVING FIELDS
           MOVE 0             TO XLN-RCURPRIN.
           INSPECT XLN-RCURPRIN REPLACING LEADING "+" BY " ".

           MOVE 0                  TO XLN-RCURINT.
           INSPECT XLN-RCURINT REPLACING LEADING "+" BY " ".
           MOVE  0                 TO XLN-RCURLC.
           INSPECT XLN-RCURLC REPLACING LEADING "+" BY " ".
           MOVE 0                  TO XLN-RCUROTH.
           INSPECT XLN-RCUROTH REPLACING LEADING "+" BY " ".
           MOVE 0                TO XLN-RPDUEPRIN.
           INSPECT XLN-RPDUEPRIN REPLACING LEADING "+" BY " ".
           MOVE 0                 TO XLN-RPDUEINT.
           INSPECT XLN-RPDUEINT REPLACING LEADING "+" BY " ".
           MOVE 0                  TO XLN-RPDUELC.
           INSPECT XLN-RPDUELC REPLACING LEADING "+" BY " ".
           MOVE 0                 TO XLN-RPDUEOTH.
           INSPECT XLN-RPDUEOTH REPLACING LEADING "+" BY " ".

           MOVE LN-ACCT-FROZEN              TO XLN-ACCT-FROZEN.

           MOVE LN-LAST-ACCRUAL-DATE        TO XLN-LAST-ACCRUAL-DATE.

           MOVE LN-TRW-SPECIAL-CODE         TO XLN-TRW-SPECIAL-CODE.
           MOVE LN-CREATION-STATUS          TO XLN-CREATION-STATUS.
           MOVE LN-TOTNODEFACT              TO XLN-TOTNODEFACT.
           INSPECT XLN-TOTNODEFACT REPLACING LEADING "+" BY " ".
           MOVE LN-YTDNODEFACT              TO XLN-YTDNODEFACT.
           INSPECT XLN-YTDNODEFACT REPLACING LEADING "+" BY " ".
           MOVE LN-TOTNOLCHG                TO XLN-TOTNOLCHG.
           INSPECT XLN-TOTNOLCHG REPLACING LEADING "+" BY " ".
           MOVE LN-PRIOR-STMT-COUNTER       TO XLN-PRIOR-STMT-COUNTER.
           MOVE LN-CONRATE                  TO XLN-CONRATE.
           MOVE LN-LBOX                     TO XLN-LBOX.
           MOVE LN-EXTRA-PRIN-LFP           TO XLN-EXTRA-PRIN-LFP.
           INSPECT XLN-EXTRA-PRIN-LFP REPLACING LEADING "+" BY " ".
           MOVE LN-REMAIN-INT-SINCE-LFP     TO XLN-REMAIN-INT-SINCE-LFP.
           INSPECT XLN-REMAIN-INT-SINCE-LFP REPLACING LEADING "+" BY " ".
           MOVE LN-REMAIN-PRIN-SINCE-LFP    TO XLN-REMAIN-PRIN-SINCE-LFP.
           INSPECT XLN-REMAIN-PRIN-SINCE-LFP
                                            REPLACING LEADING "+" BY " ".
           MOVE LN-INTERIM-COUPON           TO XLN-INTERIM-COUPON.

      * UPDATED FIELDS, 10/4/02, BLV:
           MOVE LN-MIP-FG                   TO XLN-MIP-FG.
           MOVE LN-MIP-MTD-CLAHPAID         TO XLN-MIP-MTD-CLAHPAID.
           INSPECT XLN-MIP-MTD-CLAHPAID REPLACING LEADING "+" BY " ".
           MOVE LN-MIP-CLPDTH-DATE          TO XLN-MIP-CLPDTH.
           MOVE LN-MIP-TOT-CLPAID           TO XLN-MIP-TOT-CLPAID.
           INSPECT XLN-MIP-TOT-CLPAID REPLACING LEADING "+" BY " ".
           MOVE LN-MIP-AHPDTH-DATE          TO XLN-MIP-AHPDTH.
           MOVE LN-MIP-TOT-AHPAID           TO XLN-MIP-TOT-AHPAID.
           INSPECT XLN-MIP-TOT-AHPAID REPLACING LEADING "+" BY " ".
           MOVE LN-RESCISSION               TO XLN-RESCISSION.
           MOVE LN-PLREASON                 TO XLN-PLREASON.
           MOVE LN-LAST-PYPA-DATE           TO XLN-LAST-PYPA-DATE.
           MOVE LN-PROMDAYS-PASTDUE         TO XLN-PROMDAYS-PASTDUE.
           INSPECT XLN-PROMDAYS-PASTDUE REPLACING LEADING "+" BY " ".
           MOVE LN-DLCOMM-O3-CMSI           TO XLN-DLCOMM-O3-CMSI.
           MOVE LN-DLCOMM-PP-CMSI           TO XLN-DLCOMM-PP-CMSI.

      *----------------------------------------------------------------
      *    MOVE IN VENDOR AP NUMBER FROM MAFILE:
      *----------------------------------------------------------------
LARM30*    MOVE MA-VENDOR-APPNO             TO XLN-VENDOR-APPNO.
           MOVE 0                           TO XLN-VENDOR-APPNO.
      *----------------------------------------------------------------
      *    MOVE IN FIELDS FROM NOTICE RECORD:
      *----------------------------------------------------------------
           MOVE NO-TDUE-DATE                TO XNO-TDUE.

           MOVE NO-TDUELC                   TO XNO-TDUELC.
           INSPECT XNO-TDUELC REPLACING LEADING "+" BY " ".
           MOVE NO-TDUEAMT                  TO XNO-TDUEAMT.
           INSPECT XNO-TDUEAMT REPLACING LEADING "+" BY " ".
           MOVE NO-TDUEPRIN                 TO XNO-TDUEPRIN.
           INSPECT XNO-TDUEPRIN REPLACING LEADING "+" BY " ".
           MOVE NO-TDUETBAL                 TO XNO-TDUETBAL.
           INSPECT XNO-TDUETBAL REPLACING LEADING "+" BY " ".
           MOVE NO-TDUEPOFF                 TO XNO-TDUEPOFF.
           INSPECT XNO-TDUEPOFF REPLACING LEADING "+" BY " ".

           MOVE NO-LDUE-DATE                TO XNO-LDUE.

           MOVE NO-LDUELC                   TO XNO-LDUELC.
           INSPECT XNO-LDUELC REPLACING LEADING "+" BY " ".
           MOVE NO-LDUEAMT                  TO XNO-LDUEAMT.
           INSPECT XNO-LDUEAMT REPLACING LEADING "+" BY " ".

           MOVE NO-DATE-PAID-LAST           TO XNO-DATE-PAID-LAST.

           MOVE NO-AMT-PAID-LAST            TO XNO-AMT-PAID-LAST.
           INSPECT XNO-AMT-PAID-LAST REPLACING LEADING "+" BY " ".

      * HAVE TO ADJUST LAST LP KEY SO IT FITS INTO THE A15 SIZE OF LSTLPKEY
      * FOR WORLD.  LSTLPKEY IS X(17) IN A34, X(12) IN A15
           MOVE NO-LSTLPKEY                 TO WS-LSTLPKEY.

           IF WS-LSTLP-SEQNOX NOT = SPACES
              MOVE WS-LSTLP-ACCTNO TO WS-FIX-LSTLP-ACCTNO
              MOVE WS-LSTLP-SEQNO  TO WS-FIX-LSTLP-SEQNO
              MOVE WS-FIX-LSTLPKEY TO XNO-LSTLPKEY.

           MOVE NO-NXDUE-DATE               TO XNO-NXDUE.

           MOVE NO-NXDUEAMT                 TO XNO-NXDUEAMT.
           INSPECT XNO-NXDUEAMT REPLACING LEADING "+" BY " ".
           MOVE NO-NXDUELC                  TO XNO-NXDUELC.
           INSPECT XNO-NXDUELC REPLACING LEADING "+" BY " ".
           MOVE NO-TRUCONTR                 TO XNO-TRUCONTR.
           MOVE NO-RECDEL                   TO XNO-RECENCY.

           MOVE NO-PDTH-DATE                TO XNO-PDTH.
           MOVE NO-LCPDTH-DATE              TO XNO-LCPDTH.

           MOVE NO-PRINTED                  TO XNO-PRINTED.
           MOVE NO-LCHG-INTDUE              TO XNO-LCHG-INTDUE.
           INSPECT XNO-LCHG-INTDUE REPLACING LEADING "+" BY " ".
           MOVE NO-AVAILCREDIT              TO XNO-AVAILCREDIT.
           INSPECT XNO-AVAILCREDIT REPLACING LEADING "+" BY " ".
           MOVE NO-TDUEDELINQ               TO XNO-TDUEDELINQ.
           INSPECT XNO-TDUEDELINQ REPLACING LEADING "+" BY " ".
           MOVE NO-DEFERMENT                TO XNO-DEFERMENT.
           INSPECT XNO-DEFERMENT REPLACING LEADING "+" BY " ".

           MOVE NO-TRUE-DUE-DATE            TO XNO-TRUE-DUE-DATE.

           MOVE NO-SCHED-PAYMENT            TO XNO-SCHED-PAYMENT.
           INSPECT XNO-SCHED-PAYMENT REPLACING LEADING "+" BY " ".
           MOVE NO-TDUECONTR                TO XNO-TDUECONTR.
           INSPECT XNO-TDUECONTR REPLACING LEADING "+" BY " ".
           MOVE NO-LDUECONTR                TO XNO-LDUECONTR.
           INSPECT XNO-LDUECONTR REPLACING LEADING "+" BY " ".
           MOVE NO-NXDUECONTR               TO XNO-NXDUECONTR.
           INSPECT XNO-NXDUECONTR REPLACING LEADING "+" BY " ".
           MOVE NO-CURRENT-AVAIL            TO XNO-CURRENT-AVAIL.
           INSPECT XNO-CURRENT-AVAIL REPLACING LEADING "+" BY " ".
           MOVE NO-LATE-CHG                 TO XNO-LATE-CHG.
           INSPECT XNO-LATE-CHG REPLACING LEADING "+" BY " ".
           MOVE NO-WAGE-ASSIGN-DELINQ       TO XNO-WAGE-ASSIGN-DELINQ.
           INSPECT XNO-WAGE-ASSIGN-DELINQ REPLACING LEADING "+" BY " ".
           MOVE NO-PAST-DUE-DAYS            TO XNO-PAST-DUE-DAYS.

           MOVE "Y" TO MDTE-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE                   TO XLN-ORIG-MATURE-DATE.

           MOVE SPACES TO MDTE-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE                   TO XLN-UPD-MATURE-DATE.

           MOVE LN-ACH TO XLN-ACH.
           MOVE LN-UNWIND-REASON-CODE TO XLN-UNWIND-REASON-CODE.
           MOVE LN-FROZEN-REASON      TO XLN-FROZEN-REASON.
           COMPUTE XLN-NETBAL ROUNDED = LN-CURBAL - LN-OT2BAL -
                       (LN-ANTICERN(1) - LN-ACCERN(1)) -
                       (LN-ANTICERN(2) - LN-ACCERN(2)) -
                       (LN-ANTICERN(3) - LN-ACCERN(3)) -
                       (LN-ANTICERN(5) - LN-ACCERN(5)).

           INSPECT XLN-NETBAL REPLACING LEADING "+" BY " ".

           MOVE LN-TRW-B-CONSUMER-IND TO XLN-TRW-B-CONSUMER-IND.
           MOVE LN-TRW-J-CONSUMER-IND TO XLN-TRW-J-CONSUMER-IND.
           MOVE LN-TRW-COMPLIANCE-CODE TO XLN-TRW-COMPLIANCE-CODE.

      * THESE WERE ONLY USED BY REGACC BUT COULDNT REMOVE THEM NOW
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 9
              MOVE 0 TO XNO-ALT-DEF-AMOUNT(SUB)
              INSPECT XNO-ALT-DEF-AMOUNT(SUB) 
                      REPLACING LEADING "+" BY " "
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * PR#05271 - NEED THIS ONE WITH THE CORRECT PAYMENTS TO DATE
      * FIGURE IN THE LOAN REC TO GET THE AMOUNT "TO ROLL" CONTRACTUAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LN-1STPYDATE               TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-AMT-TO-ADVANCE        TO XXX-TO-ROLL.

           MOVE X"0D" TO XLN-CR.
           MOVE X"0A" TO XLN-LF.

           ADD 1 TO XLN-KEY.
           PERFORM WRITE-XLN-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-LN.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.


           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.
      ******************************************************************
      *
      *   I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE LNEXTR-QUEUE-POS TO Q-POS.
           MOVE LNEXTR-COPIES-POS TO C-POS.
           MOVE LNEXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XLN-DIRECTORY, VALUE 13.
           MOVE XLN-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE AGE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO H-AGEDATE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE AGE-DATE TO HOLD-TODAY-DATE AGEING-DATE.
           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.

      * USE BRANCH/GROUP EXTENSIONS, #1381
           MOVE SPACES TO XLN-EXTENSION.
           IF GROUP-FLAG = "Y"
              MOVE "."       TO XLN-PATH-DOT
              MOVE ENT-GROUP TO XLN-PATH-GROUP
           ELSE
              IF NOT (BEG-BRANCH = 0 AND END-BRANCH = 9999)
                 MOVE "."       TO XLN-PATH-DOT
                 MOVE END-BRANCH TO XLN-PATH-BRNO.

           PERFORM OPEN-XLN-FILE-OUTPUT.
           PERFORM CLOSE-XLN-FILE.
           PERFORM OPEN-XLN-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      *    EO-EXEC-AUDIT SECTION.
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.


       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
      *
      *    ENTER-PARAMETERS SECTION.
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
      *----------------------------------------------------------------
       ENTER-PARM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO CREATE-NOTICE-YN.

      *----------------------------------------------------------------
       AGEDT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD AGE-DATE 
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.
           GO TO PLYN.
      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLYN.
           MOVE VDUBUF TO PL.

           IF PL = "I" OR "O"
              GO TO PLTYPEYN.

           IF PL NOT = "S"
              GO TO PLYN.

      * PL = "S"
           MOVE "B" TO PLACTIVE.
           MOVE "S  8000PLDATE." TO VDU.
           MOVE 0 TO PL-FROM-DATE TEST-DATE-PAID-LAST
                                  VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           GO TO ZBALYN.
      *----------------------------------------------------------------
       PLTYPEYN.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLTYPEYN.
           MOVE VDUBUF TO PLTYPE.
           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PLTYPEYN.
      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLTYPEYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ACTVYN.
           MOVE VDUBUF TO PLACTIVE.
           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.
      *----------------------------------------------------------------
       ENTER-PL-DATE.
           IF PL = "S"
              MOVE "S  8000PLDATE." TO VDU
              MOVE 0 TO PL-FROM-DATE TEST-DATE-PAID-LAST
                                     VDU-DATE-YYYYMMDD
              PERFORM SCREENIO
              GO TO ZBALYN.

           MOVE "ERNM060PLDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-PL-DATE.

           MOVE VDU-DATE-YYYYMMDD TO PL-FROM-DATE.
           IF PL-FROM-DATE = 0
              GO TO ENTER-PL-DATE.


      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" 
              IF PL = "S"
                 GO TO PLYN
              ELSE
                 GO TO ENTER-PL-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ZBALYN.
           MOVE VDUBUF TO ZBAL.
           IF ZBAL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ZBALYN.

       ZBAL-DATE.
           IF ZBAL = "S"
              MOVE "S  8000ZDATE." TO VDU
              MOVE 0 TO ZBAL-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO
              GO TO CREATE-NOTICE-YN.

           MOVE "ERNM060ZDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ZBAL-DATE.

           MOVE VDU-DATE-YYYYMMDD TO ZBAL-DATE.

           IF ZBAL-DATE = ZERO
              MOVE "S  8000ZDATE." TO VDU
              MOVE JULIAN-BEG TO ZBAL-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

       CREATE-NOTICE-YN.
           MOVE "E  A010XNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO CREATE-NOTICE-YN.
           MOVE VDUBUF TO CREATE-NOTICE.
           IF CREATE-NOTICE NOT = "Y" AND NOT = "N" 
              GO TO CREATE-NOTICE-YN.


      *----------------------------------------------------------------
       EXIT-PARM.
           EXIT.

      ******************************************************************
      *
      *     DISPLAY-PARAMETERS SECTION.
      *
      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PLTYPE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8000PLDATE." TO VDU.
           MOVE PL-FROM-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8000ZDATE." TO VDU.
           MOVE ZBAL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010XNO." TO VDU.
           MOVE CREATE-NOTICE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      ******************************************************************
      *
      *     W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING CLASS       :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L              :" TO RANGE-LINE.
           MOVE PL                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L TYPE         :" TO RANGE-LINE.
           MOVE PLTYPE                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTIVE             :" TO RANGE-LINE.
           MOVE PLACTIVE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L FROM DATE    :" TO RANGE-LINE.
           MOVE PL-FROM-DATE           TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "3+ YEARS LAST DATE :" TO RANGE-LINE.
           MOVE TEST-DATE-PAID-LAST     TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO BALANCES      :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO BALANCES FROM :" TO RANGE-LINE.
           MOVE ZBAL-DATE              TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ******************************************************************
      *
      *        HEAD PAGE ROUTINE
      *
      ******************************************************************
       HEAD-PAGE SECTION.

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *----------------------------------------------------------------
       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *************************************************************
      *    C L A S S   S C A N   R O U T I N E
      *************************************************************
       CLASS-SCAN SECTION.
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

           
           

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBLP/ADDONSPR.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO LNEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNEXTR-SYSDATE.
           DISPLAY LNEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LNEXTR-FORM-FIELDS TO WR-BUF.
           MOVE LNEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LNEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LNEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *     M I S C E L L E N O U S    P A R A G R A P H S
      ******************************************************************
       MISC-PARA SECTION.
      *----------------------------------------------------------------
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
      *COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *----------------------------------------------------------------
       COMPUTE-PAST-DUE-AMT.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE AGE-DATE     TO CEPP2-DATE.
           MOVE LN-MAKERCD(1) TO CEPP-MAKERCD.
           PERFORM TOTAL-DELINQUENCY.
           IF CDEL-ARREARAGE < 0
              MULTIPLY -1 BY CDEL-ARREARAGE
           ELSE
              MOVE 0 TO CDEL-ARREARAGE.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
      *CONDITIONAL-PRINT.
      *    IF DETAIL-LINE NOT = " "
      *       MOVE 1 TO LN-FEED
      *       PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *    I/O   R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXLNI.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D    P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *----------------------------------------------------------------
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *----------------------------------------------------------------
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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME
           ELSE
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE SPACES TO MESS
                 PERFORM SEND-LEGEND.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LNEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
