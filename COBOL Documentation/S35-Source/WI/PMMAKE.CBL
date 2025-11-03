       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PMMAKE.
      ******************************************************************
      *              WI/PMMAKE
      *
      * NOTE: I HAD TO CHANGE THE FORM IN A30 ONLY TO HAVE FLDSEL FIELDS
      *       NAME 010 - 080 SO I HAD TO CHANGE THE INPUT FIELDS TO BE
      *            F010 - F090
      *
      *  DESCRIPTION:   BUILD LOAN CONVERSION PAYMENTS
      *
      * REV:
      *  CLS  061002 CALLED FROM LONIF1, ALLOW ENTRY OF PAYMENTS PRIOR TO THE
      *              'BF' TRANSACTION... ONLY CREATES LP, SET TO VOID SO CAN'T
      *              BE REVERSED THROUGH LOAN PROCESSING. PR#2659
      *  BAH 170420 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 190103 GLOBAL LPFILE
      *  BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 20220803 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKI.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY              PIC 9(4).
           03  WK-LP1-KEY.
               05  WK-LP-ACCTNO    PIC 9(8).
               05  WK-LP-SEQNO     PIC 9(5).
           03  WK-DATE             PIC 9(8).
           03  WK-AMOUNT           PIC S9(7)V99.
           03  WK-REFNO            PIC 9(5).
           03  WK-APINT            PIC S9(7)V99.
           03  WK-APLC             PIC S9(7)V99.
           03  WK-APOTH            PIC S9(7)V99.
           03  WK-APOT2            PIC S9(7)V99.
           03  WK-APCUR            PIC S9(7)V99.
           03  WK-OTHBAL           PIC S9(7)V99.
           03  WK-OT2BAL           PIC S9(7)V99.
           03  WK-INTBAL           PIC S9(7)V99.
           03  WK-LCBAL            PIC S9(7)V99.
           03  WK-CURBAL           PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/PMMAKE.CBL S35 02/22/24-12:51:37 >".
      *******************************
      *       PROGRAM WORKERS       *
      *******************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
      * ORIGINAL LOAN RECORD
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "LPOR".
      * CHANGED LOAN RECORD
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "LPCH".

       01  WK-IFN                   PIC X(2) VALUE "WK".
       01  HOLD-WK-KEY              PIC 9(4) VALUE ZEROES.

       01  PMMAKE-LINES              PIC 99  VALUE 22.
       01  PMMAKE-COLUMNS            PIC 99  VALUE 80.
       01  PMMAKE-BEGIN-Y            PIC 99  VALUE 1.
       01  PMMAKE-BEGIN-X            PIC 99  VALUE 1.
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.


       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).

       01  MSEL-LIST         PIC X(5) VALUE "MSEL.".
       01  BF-EXISTS-FG      PIC X      VALUE SPACES.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.

       01  CURRENT-PAGE      PIC S999.
       01  PAGE-COUNT        PIC 999.
       01  MAX-PAGES         PIC 999 VALUE 10.
       01  PAGE-KEY-01.
           03  PAGE-KEY      OCCURS 08 PIC 9(12).

       01  CHAN          PIC X(4).
       01  ERRCD         PIC X.

       01  NEXT-NEW-LP-SEQNO  PIC 9(5) VALUE ZEROS.

       01  LEGEND             PIC X(70).

      * DO NOT ADD BRNO TO THESE HOLD KEYS
       01  HOLD-LP1-KEY                 VALUE SPACES.
           03  HOLD-LP1-ACCTNO          PIC 9(8).
           03  HOLD-LP1-SEQNO           PIC 9(5).

       01  PREV-LP1-KEY                 VALUE SPACES.
           03  PREV-LP1-ACCTNO          PIC 9(8).
           03  PREV-LP1-SEQNO           PIC 9(5).

       01  ORIGINAL-LP-REC    PIC X(LP-SIZE)   VALUE SPACES.
       01  CHANGED-LP-REC     PIC X(LP-SIZE)   VALUE SPACES.

       01  DISPLAY-FIELD-LIST.
           03  FILLER              PIC X(50)  VALUE
           "F010 F020 F030 F040 F041 F050 F051 F060 F070 F071 ".
           03  FILLER              PIC X(15)  VALUE
           "F080 F081 F090.".

      ******************************
      *    SCREEN DISPLAY BUFFER   *
      ******************************
       01  DISPLAY-FIELD-BUF     VALUE SPACES.
           03  F01         PIC 99/99/99 BLANK ZERO.
           03  F02         PIC ZZZZZZZ.ZZ-.
           03  F03         PIC X(5).
           03  F04         PIC ZZZZ9.ZZ-.
           03  F041        PIC ZZZZ9.ZZ-.
           03  F05         PIC ZZZZ9.ZZ-.
           03  F051        PIC ZZZZ9.ZZ-.
           03  F06         PIC ZZZZZ9.ZZ-.
           03  F07         PIC ZZZZ9.ZZ-.
           03  F071        PIC ZZZZ9.ZZ-.
           03  F08         PIC ZZZZ9.ZZ-.
           03  F081        PIC ZZZZ9.ZZ-.
           03  F09         PIC ZZZZZZ9.ZZ-.

       01 DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
            "010 020 030 040 050 060 070 080 ".
           03  FILLER      PIC X(25) VALUE
            "ENDMORE:F PAGENOX PAGENO.".

       01  DISPLAY-BUF.
           03 D-LINE    OCCURS 08 TIMES.
              05 D-KEY          PIC 9(4).
              05 FILLER         PIC X.
              05 D-LP-ACCTNO    PIC 9(8).
              05 FILLER         PIC X.
              05 D-LP-SEQNO     PIC 9(4).
              05 FILLER         PIC X.
              05 D-DATE         PIC 99/99/99.
              05 D-AMOUNT       PIC ZZZZZZ9.99-.
              05 FILLER         PIC X.
              05 D-CURBAL       PIC ZZZZZZ9.99.

           03  D-END-MORE       PIC X(7).
           03  D-PAGE-PROMPT    PIC X(5).
           03  D-PAGENO         PIC ZZ9.



       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(32)  VALUE
            "010 020 030 040 050 060 070 080.".
       01  FLDSEL-BUF.
           03  FLDSEL-KEY          PIC 9(04).
           03  FILLER              PIC X.
           03  FLDSEL-LP-ACCTNO    PIC 9(8).
           03  FILLER              PIC X.
           03  FLDSEL-LP-SEQNO     PIC 9(4).
           03  FILLER              PIC X(31).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "080.".

      ********************************
      *    ELEMENT SPECIFICATIONS    *
      ********************************
       78  MAX                      VALUE 9.
       01  SPEC-TABLE.
      * 1. PAYMENT DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 2. LP-TRAMT
           03  FILLER      PIC X(8) VALUE "ENNS072 ".
      * 3. REFERENCE
           03  FILLER      PIC X(8) VALUE "E  A050 ".
      * 4.  LP-APINT APLC
           03  FILLER      PIC X(8) VALUE "ENNS052 ".
      * 5.  LP-APOTH/LP-ATOT2
           03  FILLER      PIC X(8) VALUE "ENNS052 ".
      * 6.  LP-APCUR
           03  FILLER      PIC X(8) VALUE "ENNS062 ".
      * 7.  LP-OTHBAL
           03  FILLER      PIC X(8) VALUE "ENNS052 ".
      * 8.  LP-INT/LATE BAL
           03  FILLER      PIC X(8) VALUE "ENNS052 ".
      * 9.  CURBAL
           03  FILLER      PIC X(8) VALUE "ENNS062 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       01 PMMAKE-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PMMAKE_DEF.CPY".
       COPY "LIBWI/PMMAKE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/PMMAKEW.CPY".
 
       SCREEN SECTION.
       COPY "LIBWI/PMMAKE_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME PMMAKE-BUF EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           CLOSE WK-FILE.

       COPY "LIBGB/DECLAR2.CPY".

      **********************************
      *      MAIN PROGRAM MODULE
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PMMAKE" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE PMMAKE-OWNBR  TO LN-OWNBR.
           MOVE PMMAKE-ACCTNO TO LN-ACCTNO.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE PMMAKE-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  PMMAKE-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    PMMAKE-COLUMNS + VDU-WIN-W-MOD
                   LINES   PMMAKE-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS PMMAKE-WINDOW-HANDLE.

           MOVE ZEROES TO PREV-LP1-KEY.

       MAIN-MODULE.
      D    STOP MESS.
           IF IO-TYPE = "CLR"
              GO TO END-ROUTINE.

           PERFORM FORM-RESET.

           PERFORM REC-LP-MODULE.

           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM SET-REMAINING-LP-FIELDS
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.


      **************************************
       REC-LP-MODULE SECTION.
      **************************************
       REC-LP-MODULE-BEGIN.

           MOVE 0 TO LP-BRNO LP-ACCTNO LP-SEQNO.
           MOVE " " TO IO-TYPE.
           MOVE 0   TO NEW-FLAG.

           IF ( PREV-LP1-KEY NOT = ZEROES )
              MOVE PREV-LP1-KEY TO HOLD-LP1-KEY
              MOVE ZEROES       TO PREV-LP1-KEY
              GO TO LOAD-SCREEN-FROM-KEY.

       REC-LP-ACCTNO.
           IF ( PMMAKE-ACCTNO = ZEROES )
              MOVE "EN N060LPNO." TO VDU
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN060LPNO." TO VDU
              MOVE PMMAKE-ACCTNO TO VDUNUM0
              PERFORM SCREENIO
              MOVE VDU---ENTER-KEY TO VDUSTATUS.

           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LP-MODULE.

           IF VDUSTATUS NOT = "00"
             GO TO REC-LP-ACCTNO.


           MOVE VDUNUM0 TO LN-ACCTNO LP-ACCTNO HOLD-LP1-ACCTNO.

           IF LP-ACCTNO = 0
              GO TO REC-LP-ACCTNO.

           PERFORM GET-LN.
           IF IO-BAD
              MOVE "LOAN MASTER DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LP-MODULE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-BAD
              MOVE "NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              IF ( PMMAKE-ACCTNO = ZEROES )
                 MOVE "CLR" TO IO-TYPE
                 GO TO REC-LP-ACCTNO
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-REC-LP-MODULE.

      *=================================================================
       LOAD-SCREEN-FROM-KEY.

           MOVE "SN N060LPNO." TO VDU.
           MOVE LN-ACCTNO      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  X110SSNO." TO VDU.
           MOVE BW-SSNO TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A024MAKER1." TO VDU.
           MOVE SPACES TO VDUBUF.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                  INTO VDUBUF.
           PERFORM SCREENIO.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM FIND-LAST-PAYMENT-SEQNO.

           IF ( HOLD-WK-KEY = 1 )
                MOVE 01 TO HOLD-LP1-SEQNO
                GO TO LOAD-RECORD-FROM-KEY.


           MOVE "S  A460SCAN-HEADER." TO VDU.
           MOVE "      ACCT  #  SEQ     DATE    AMOUNT     BALANCE"
                                                     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040WSEL." TO VDU.
           MOVE "F6-MORE INFO, F1-CANCEL, ENTER" TO VDUBUF.
           PERFORM SCREENIO.


           PERFORM BUILD-PAGE-TABLE.

           IF ( F1-KEY )
              IF ( PMMAKE-ACCTNO = ZEROES )
                 GO TO REC-LP-MODULE-BEGIN
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-REC-LP-MODULE.

           MOVE FLDSEL-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF FLDSEL-KEY = "****"
              MOVE NEXT-NEW-LP-SEQNO TO HOLD-LP1-SEQNO
              MOVE SPACES TO WR-BUF
              MOVE DISPLAY-FIELD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE WK-LP-SEQNO  TO HOLD-LP1-SEQNO.

           IF ( F7-KEY )
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LP-MODULE.

      *=================================================================
       LOAD-RECORD-FROM-KEY.

           MOVE LN-OWNBR        TO LP-BRNO.
           MOVE HOLD-LP1-ACCTNO TO LP-ACCTNO.
           MOVE HOLD-LP1-SEQNO  TO LP-SEQNO.

           PERFORM READ-LP1-FILE.
           IF ( IO-BAD )
              MOVE 1 TO NEW-FLAG
              PERFORM NEW-RECORD-SETUP
           ELSE
              PERFORM DISPLAY-FIELDS
              MOVE LP-REC TO ORIGINAL-LP-REC.

       END-REC-LP-MODULE.


      **********************************************
       FIND-LAST-PAYMENT-SEQNO SECTION.
      **********************************************
           MOVE ZEROES TO HOLD-WK-KEY.
           MOVE ZEROES TO NEXT-NEW-LP-SEQNO.

      * CREATE FIRST RECORD 

           IF LN-CURBAL = ZEROS
              MOVE SPACES      TO WK-REC
              ADD  01          TO HOLD-WK-KEY
              MOVE HOLD-WK-KEY TO WK-KEY
              MOVE ZEROES      TO WK-LP-ACCTNO
              MOVE ZEROES      TO WK-LP-SEQNO
              PERFORM WRITE-WK-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           PERFORM START-LP1-FILE-NOT-GREATER.
       FIND-PAYMENT-SEQNO-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD
              GO TO FIND-LAST-PAYMENT-SEQNO-EXIT.

           IF (LP-BRNO NOT = LP-PATH-OWNBR) OR
                (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO FIND-LAST-PAYMENT-SEQNO-EXIT.

           IF ( NEXT-NEW-LP-SEQNO < LP-SEQNO )
              COMPUTE NEXT-NEW-LP-SEQNO = LP-SEQNO + 1.

           IF LP-TRCD NOT = "XX"
              GO TO FIND-PAYMENT-SEQNO-NEXT.

           IF LP-TRCD = "BF"
              MOVE "Y" TO BF-EXISTS-FG
              GO TO FIND-PAYMENT-SEQNO-NEXT.

           PERFORM CREATE-WK-FILE.
           GO TO FIND-PAYMENT-SEQNO-NEXT.

       FIND-LAST-PAYMENT-SEQNO-EXIT.
           EXIT.


      ******************************************
       CREATE-WK-FILE SECTION.
      *****************************************

      * PM PAYMENT RECORDS THAT EXIST IN LP

           MOVE SPACES      TO WK-REC.

           ADD  01          TO HOLD-WK-KEY.

           MOVE HOLD-WK-KEY TO WK-KEY.
           MOVE LP-ACCTNO   TO WK-LP-ACCTNO.
           MOVE LP-SEQNO    TO WK-LP-SEQNO.
           MOVE LP-TRDATE   TO WK-DATE.
           MOVE LP-TRAMT    TO WK-AMOUNT.
           MOVE LP-REFNO    TO WK-REFNO.
           MOVE LP-APINT    TO WK-APINT.
           MOVE LP-APLC     TO WK-APLC.
           MOVE LP-APOTH    TO WK-APOTH.
           MOVE LP-APOT2    TO WK-APOT2.
           MOVE LP-APCUR    TO WK-APCUR.
           MOVE LP-OTHBAL   TO WK-OTHBAL.
           MOVE LP-INTBAL   TO WK-INTBAL.
           MOVE LP-LCBAL    TO WK-LCBAL.
           MOVE LP-CURBAL   TO WK-CURBAL.

           PERFORM WRITE-WK-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       CREATE-WK-FILE-EXIT.
           EXIT.


      *******************************************
       BUILD-PAGE-TABLE SECTION.
      *******************************************
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-WK-FILE.
           MOVE 0 TO PAGE-KEY(PAGE-COUNT).
       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO WK-KEY.
           PERFORM START-WK-FILE.

       BUILD-PAGENO.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 8
              GO TO DISPLAY-PAGE.

           IF LN-CURBAL = ZEROS
              IF SUB = 1
                 MOVE "********CREATE NEW********" TO D-LINE(1)
                 GO TO SKIP-NEW-RECORD.

           MOVE WK-KEY       TO D-KEY(SUB).
           MOVE WK-LP-ACCTNO TO D-LP-ACCTNO(SUB).
           MOVE WK-LP-SEQNO  TO D-LP-SEQNO(SUB).
           MOVE WK-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(SUB).
           MOVE WK-AMOUNT    TO D-AMOUNT(SUB).
           MOVE WK-CURBAL    TO D-CURBAL(SUB).
        SKIP-NEW-RECORD.
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 GO TO BUILD-PAGE-TABLE-END.

           MOVE "MORE..." TO D-END-MORE.
           MOVE "PAGE:" TO D-PAGE-PROMPT.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE WK-KEY  TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE "F7 EXIT, SELECT CODE(RTN):" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST F1-KEY
           IF FLDSEL-STAT = "10"
              MOVE VDU---F1-KEY TO VDUSTATUS
              GO TO BUILD-PAGE-TABLE-END.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1<"
              MOVE LP-PATH-OWNBR    TO LP-BRNO
              MOVE FLDSEL-LP-ACCTNO TO LP-ACCTNO
              MOVE FLDSEL-LP-SEQNO  TO LP-SEQNO
              PERFORM READ-LP1-FILE
              PERFORM DISPLAY-FIELDS
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF NOT (FLDSEL-STAT = "00" )
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * FLDSEL-BUF IS RETURN USING A ENTER-KEY

       BUILD-PAGE-TABLE-END.
           MOVE SPACES TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE "SCAN-HEADER." TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE "WSEL." TO WR-LIST.
           PERFORM CALL-WRFORM.

       BUILD-PAGE-TABLE-EXIT.
           EXIT.


      *****************************************
       SET-REMAINING-LP-FIELDS SECTION.
      *****************************************
           MOVE "XX"                TO LP-TRCD.
      *    MOVE "Y"                 TO LP-REV.
           MOVE 999999999           TO LP-SSNO.
           MOVE EXT-CONAME-USER-LOC TO LP-POSTING-BR.

       SET-REMAINING-LP-FIELDS-EXIT.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
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
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE SPACES TO VDUFIELD.
           STRING "F",ELE,"0","." DELIMITED BY " " INTO VDUFIELD.
      *    MOVE ELE TO VDUROW.
      *    MOVE 0 TO VDUCOL.
      *    MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-UP.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-DOWN.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO LP-TRDATE LP-PAYDATE LP-POSTDATE.
           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUNUM2 TO LP-TRAMT.
           GO TO CREATE-ROUTINE.

       F-03.
           MOVE VDUBUF TO LP-REFNO.
           GO TO CREATE-ROUTINE.

       F-04.
           MOVE VDUNUM2 TO LP-APINT.
       F-041.
           MOVE "ENNS052F041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-041.
           MOVE VDUNUM2 TO LP-APLC.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO LP-APOTH.
       F-051.
           MOVE "ENNS052F051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-051.
           MOVE VDUNUM2 TO LP-APOT2.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO LP-APCUR.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO LP-OTHBAL.
       F-071.
           MOVE "ENNS052F071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-071.
           MOVE VDUNUM2 TO LP-OT2BAL.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO LP-INTBAL.
       F-081.
           MOVE "ENNS052F081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-081.
           MOVE VDUNUM2 TO LP-LCBAL.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO LP-CURBAL.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.


       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************
       WRITE-MODULE SECTION.
      *****************************
           PERFORM OPEN-LP1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LP-REC TO CHANGED-LP-REC.
           IF ORIGINAL-LP-REC = CHANGED-LP-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LP-REC NOT = ORIGINAL-LP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-LP-REC TO LP-REC.

           PERFORM REWRITE-LP1-FILE.

           MOVE ZEROES TO PREV-LP1-KEY.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LP1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE LP-ACCTNO TO PREV-LP1-ACCTNO
              MOVE LP-SEQNO  TO PREV-LP1-SEQNO
              ADD  01        TO PREV-LP1-SEQNO.

       END-WRITE-MODULE.
           PERFORM CLOSE-LP1-FILE.

      *********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *********************************************
           MOVE ORIGINAL-LP-REC TO LPOR-REC.
           MOVE CHANGED-LP-REC  TO LPCH-REC.

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LPOR-TRCD NOT = LPCH-TRCD
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRCD    "              TO WORK-ERRLOG-DESC
             MOVE LPOR-TRCD               TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-TRCD               TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-TRDATE NOT = LPCH-TRDATE
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRDATE  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-TRDATE             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-TRDATE             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-PAYDATE NOT = LPCH-PAYDATE
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "PAYDATE "              TO WORK-ERRLOG-DESC
             MOVE LPOR-PAYDATE            TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-PAYDATE            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-POSTDATE NOT = LPCH-POSTDATE
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "POSTDATE"              TO WORK-ERRLOG-DESC
             MOVE LPOR-POSTDATE           TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-POSTDATE           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-POSTTIME NOT = LPCH-POSTTIME
             MOVE "041"                   TO WORK-ERRLOG-FIELD
             MOVE "POSTTIME"              TO WORK-ERRLOG-DESC
             MOVE LPOR-POSTTIME           TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-POSTTIME           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-POSTING-BR NOT = LPCH-POSTING-BR
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "POST BR "              TO WORK-ERRLOG-DESC
             MOVE LPOR-POSTING-BR         TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-POSTING-BR         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-USERID NOT = LPCH-USERID
             MOVE "051"                   TO WORK-ERRLOG-FIELD
             MOVE "USER ID "              TO WORK-ERRLOG-DESC
             MOVE LPOR-USERID             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-USERID             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REFNO NOT = LPCH-REFNO
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "REFNO   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-REFNO              TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-REFNO              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REV NOT = LPCH-REV
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "REV     "              TO WORK-ERRLOG-DESC
             MOVE LPOR-REV                TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-REV                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REVTRCD NOT = LPCH-REVTRCD
             MOVE "071"                   TO WORK-ERRLOG-FIELD
             MOVE "REVTRCD "              TO WORK-ERRLOG-DESC
             MOVE LPOR-REVTRCD            TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-REVTRCD            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REVSEQNO NOT = LPCH-REVSEQNO
             MOVE "072"                   TO WORK-ERRLOG-FIELD
             MOVE "REVSEQNO"              TO WORK-ERRLOG-DESC
             MOVE LPOR-REVSEQNO           TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-REVSEQNO           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-COLLECTOR NOT = LPCH-COLLECTOR
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "COLLEC  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-COLLECTOR          TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-COLLECTOR          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-STATUSFG NOT = LPCH-STATUSFG
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "STATUSFG"              TO WORK-ERRLOG-DESC
             MOVE LPOR-STATUSFG           TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-STATUSFG           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-TILLNO NOT = LPCH-TILLNO
             MOVE "091"                   TO WORK-ERRLOG-FIELD
             MOVE "TILLNO  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-TILLNO             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-TILLNO             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-IBPC NOT = LPCH-IBPC
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "IBPC    "              TO WORK-ERRLOG-DESC
             MOVE LPOR-IBPC               TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-IBPC               TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-MANUAL-RB NOT = LPCH-MANUAL-RB
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "MANUALRB"              TO WORK-ERRLOG-DESC
             MOVE LPOR-MANUAL-RB          TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-MANUAL-RB          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-PDTH-DATE NOT = LPCH-PDTH-DATE
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "PDTH    "              TO WORK-ERRLOG-DESC
             MOVE LPOR-PDTH-DATE          TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-PDTH-DATE          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LCPDTH-DATE NOT = LPCH-LCPDTH-DATE
             MOVE "121"                   TO WORK-ERRLOG-FIELD
             MOVE "LCPDTH-DATE  "         TO WORK-ERRLOG-DESC
             MOVE LPOR-LCPDTH-DATE        TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-LCPDTH-DATE        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-NOLCHG NOT = LPCH-NOLCHG
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "NOLCHG  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-NOLCHG             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-NOLCHG             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-TRAMT NOT = LPCH-TRAMT
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRAMT   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-TRAMT              TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-TRAMT              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APINT NOT = LPCH-APINT
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "APINT   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APINT              TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-APINT              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APLC NOT = LPCH-APLC
             MOVE "191"                   TO WORK-ERRLOG-FIELD
             MOVE "APLC    "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APLC              TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-APLC              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APOTH NOT = LPCH-APOTH
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "APOTH   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APOTH              TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-APOTH              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APOT2 NOT = LPCH-APOT2
             MOVE "201"                   TO WORK-ERRLOG-FIELD
             MOVE "APOT2   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APOT2              TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-APOT2              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APCUR NOT = LPCH-APCUR
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "APCUR   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APCUR              TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-APCUR              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-OTHBAL NOT = LPCH-OTHBAL
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "OTHBAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-OTHBAL             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-OTHBAL             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-OT2BAL NOT = LPCH-OT2BAL
             MOVE "221"                   TO WORK-ERRLOG-FIELD
             MOVE "OT2BAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-OT2BAL             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-OT2BAL             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-INTBAL NOT = LPCH-INTBAL
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTBAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-INTBAL             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-INTBAL             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LCBAL NOT = LPCH-LCBAL
             MOVE "231"                   TO WORK-ERRLOG-FIELD
             MOVE "LC BAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-LCBAL              TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-LCBAL              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-CURBAL NOT = LPCH-CURBAL
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "CURBAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-CURBAL             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-CURBAL             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-INTPAID NOT = LPCH-INTPAID
             MOVE "28 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTPAID "              TO WORK-ERRLOG-DESC
             MOVE LPOR-INTPAID            TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-INTPAID            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-INTDUE NOT = LPCH-INTDUE
             MOVE "29 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTDUE  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-INTDUE             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-INTDUE             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-NEWSTAT-RATE NOT = LPCH-NEWSTAT-RATE
             MOVE "31 "                   TO WORK-ERRLOG-FIELD
             MOVE "STATRATE"              TO WORK-ERRLOG-DESC
             MOVE LPOR-NEWSTAT-RATE       TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-NEWSTAT-RATE       TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLPROC NOT = LPCH-DLPROC
             MOVE "33 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLPROC  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLPROC             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-DLPROC             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLPARVPAID-ADJ NOT = LPCH-DLPARVPAID-ADJ
             MOVE "341"                   TO WORK-ERRLOG-FIELD
             MOVE "PARVPAID"              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLPARVPAID-ADJ     TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-DLPARVPAID-ADJ     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLRVCHGS-ADJ(1) NOT = LPCH-DLRVCHGS-ADJ(1)
             MOVE "35 "                   TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 1"              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLRVCHGS-ADJ(1)    TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-DLRVCHGS-ADJ(1)    TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLRVCHGS-ADJ(2) NOT = LPCH-DLRVCHGS-ADJ(2)
             MOVE "351"                   TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 2"              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLRVCHGS-ADJ(2)    TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-DLRVCHGS-ADJ(2)    TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLRVCHGS-ADJ(3) NOT = LPCH-DLRVCHGS-ADJ(3)
             MOVE "352"                   TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 3"              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLRVCHGS-ADJ(3)    TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-DLRVCHGS-ADJ(3)    TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

      *****************************
       LOG-FIELD-CHANGE SECTION.
      *****************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ************************************
       DELETE-MODULE SECTION.
      ************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LP-REC NOT = ORIGINAL-LP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LP1-FILE.

      *********************************
       NEW-RECORD-SETUP SECTION.
      *********************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LP-FILE.

           MOVE LP-PATH-OWNBR   TO LP-BRNO.
           MOVE HOLD-LP1-ACCTNO TO LP-ACCTNO.
           MOVE HOLD-LP1-SEQNO  TO LP-SEQNO.
           MOVE 0 TO ELE.

       NEW-RECORD-EXIT.
           EXIT.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE SPACES TO DISPLAY-FIELD-BUF.

           MOVE LP-TRDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F01.
           MOVE LP-TRAMT TO F02.
           MOVE LP-REFNO TO F03.
           MOVE LP-APINT TO F04.
           MOVE LP-APLC TO F041.
           MOVE LP-APOTH TO F05.
           MOVE LP-APOT2 TO F051.
           MOVE LP-APCUR TO F06.

           MOVE LP-OTHBAL TO F07.
           MOVE LP-OT2BAL TO F071.
           MOVE LP-INTBAL TO F08.
           MOVE LP-LCBAL TO F081.
           MOVE LP-CURBAL TO F09.

       DISPLAY-FIELDS-EXIT.
           MOVE DISPLAY-FIELD-BUF TO WR-BUF.
           MOVE DISPLAY-FIELD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON PMMAKE-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY PMMAKE-SCREEN UPON PMMAKE-WINDOW-HANDLE.
           MOVE PMMAKE-FORM-FIELDS TO WR-BUF.
           MOVE PMMAKE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE PMMAKE-HELP--FILE TO HELP-LINK-FILE.
           MOVE PMMAKE-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PMMAKE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************************
       MISC-PARAGRAPHS SECTION.
      **********************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-Y-OR-N.
           MOVE "ENTER: 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       ENTER-Y-OR-SP.
           MOVE "ENTER: 'Y' OR ' '" TO MESS.
           PERFORM SEND-MESS.

       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBLP/LPLPCL.CPY".

      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      **********************************************
       END-ROUTINE SECTION.
      **********************************************
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           DESTROY PMMAKE-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
