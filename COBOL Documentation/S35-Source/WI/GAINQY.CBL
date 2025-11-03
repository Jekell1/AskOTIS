      *** NOTE NEEDS WORK WITH AUDIQ1 AND NEXT ACCT
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GAINQY.
       DATE-WRITTEN.    071495.
      *****************************************************
      *                 (WI)
      *  DESCRIPTION:  GEORGIA INFORMATION FROM F9 INTERRUPT IF LN-ORGST "GA"
      *                  BREAKS OUT 4%, 8% AND CLOSING FEE
      * REV:
      *  MJD 082195 ADDED FIELDS FOR PRIOR LOAN DATE AND PRIOR
      *             PAYOFF BALANCE.
      *  JTG ADDED D-PRIOR-PAYMENT & D-TOT-REPAY
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  BLV 081297 IF LOAN IS TRANSFERRED, GET PRIOR LN INFO
      *             FROM ORIG BRANCH IF ORIG BRANCH IS GA
      *  MJD 990726 CHAGED TO DISPLAY UP TO 3 OTHER OPEN SMALL LOANS.
      *             CHANGED TO PRINT UP TO 4 FEE'S AND DESCRIPTIONS
      *             OR 3 FEE'S AND DESCRIPTIONS AND AN OVERFLOW (IF >4
      *              FEES ARE ADDED TO A LOAN).
      *  MJD 990812 ADDED GAINQ-STAT TO PASS BACK EXIT STATUS OF WINDOW.
      *  MJD 000405 FIXED BUG: LE-BALANCE WAS PRINTING IN THE PRIOR PAID OFF
      *             DATE FIELD.
      *  BLV 020628 COPIED THIS PROGRAM FROM A10.  MIKE NEVER DID CHANGES
      *             FROM FALL OF 1999 IN A15, LOOKED TO ME LIKE ALMOST
      *             ALL THE DIFFERENCES WERE THESE CHANGES DONE IN A10
      *  BLF 070918 ADDED ARCHIVE PATH LOGIC FOR LN & LE, TO GET INFO
      *             FROM ARCHIVE IF GAINQ-ARCHIVE-FG = "Y", WORLD PR# 547
      *  BAH 130730 ADDED CLOSING-FEE (LN-POINTS) FOR WORLD PR# 898
      *
      * KEC 2016.0318 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  CS 160509 REMOVED REDEF LE-FLAG/LE-SSNO AND MADE SEPARTE FIELDS
      *  BAH 170418 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181030 COMMENTED OUT ARCHIVE STUFF
      *  BAH 181218 GLOBAL LNFILE
      *  BAH 190128 SPLIT CDFILE
      *  BAH 190417 D-PRIOR-ACCTNO AND D-OTHER-LOAN2 WERE BEING DIVIDED
      *             BY 100 TO REMOVE THE 01 FROM A15, BUT CAUSED
      *             TRUNCATION IN A30, QA0006
      * BAH 20191007 REMOVED ACCESS-CALL LNFILE
      * BAH 20220826 HARD CODED START
      * BAH 20230714 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      *******************************************************************
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/GAINQY S35 04/17/24-12:52:29 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LNFILE".

      *********************************************************
      *   LEFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LE-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LE-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LE-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LE-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LE-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LEFILE".

       01  GAINQ-LINES               PIC 99  VALUE 20.
       01  GAINQ-COLUMNS             PIC 99  VALUE 80.
       01  GAINQ-BEGIN-Y             PIC 99  VALUE 1.
       01  GAINQ-BEGIN-X             PIC 99  VALUE 3.

       01  ERRCD           PIC XX   VALUE " ".
       01  ELE             PIC 9(6).
       01  SUB             PIC 9(6) COMP-3.
       01  SUB2            PIC 9(6) COMP-3.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).
       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

       01  HOLD-FEE-INFO OCCURS 4.
           03  HOLD-FEECODE  PIC X(02)    VALUE SPACES.
           03  HOLD-FEE-DESCRIPTION.
               05  HOLD-FEEDESC  PIC X(16)    VALUE SPACES.
               05  HOLD-FEECOLON PIC X(01)    VALUE ":".
           03  HOLD-FEEAMT   PIC S9(7)V99 VALUE ZEROS.

       01  SAVE-LN-PATH    PIC X(35).

       01  HOLD-LN-ACCTNO  PIC 9(08).
       01  HOLD-LN-SSNO    PIC 9(9).
       01  HOLD-LN-CLASS   PIC 9(3).
       01  HOLD-BW-LOANS   PIC 9(02).
       01  OTHER-ACCTNO    PIC 9(8) OCCURS 3.
      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************

       01  DISPLAY-BUF1        VALUE " ".
           03  D-010-040       OCCURS 3.
               05  D-SSNO      PIC X(11).
               05  D-NAME      PIC X(26).
               05  D-CODE      PIC X(1).
           03  D-OTHER-LOAN-LINE.
               05  D-OTHER-LOAN-1       PIC ZZZZZZ BLANK ZERO.
               05  D-OTHER-LOAN-SLASH2  PIC X(01).
               05  D-OTHER-LOAN-2       PIC ZZZZZZ BLANK ZERO.
               05  D-OTHER-LOAN-SLASH3  PIC X(01).
               05  D-OTHER-LOAN-3       PIC ZZZZZZ BLANK ZERO.
               05  FILLER               PIC X(05).
           03  D-OTHER-LOAN-LINE2 REDEFINES D-OTHER-LOAN-LINE.
               05  D-OTHER-LOAN2        PIC ZZZZZZ    BLANK ZERO.
               05  FILLER               PIC X(01).
               05  D-OTHER-LOANDATE2    PIC 99/99/99  BLANK ZERO.
               05  FILLER               PIC X(01).
               05  D-OTHER-CURBAL2      PIC $$$$$9.99 BLANK ZERO.
           03  D-COL-CODE      PIC X(2).
           03  D-COL-DESC      PIC X(15).
           03  D-SRC-CODE      PIC X(2).
           03  D-SRC-DESC      PIC X(15).
           03  D-FIN-CHGS      PIC ZZZZZ9.99  BLANK ZERO.
           03  D-INT-CHGS      PIC ZZZZZ9.99  BLANK ZERO.
           03  D-FEE-4         PIC ZZZZZ9.99  BLANK ZERO.
           03  D-FEE-8         PIC ZZZZZ9.99  BLANK ZERO.
           03  D-CLOSING       PIC ZZZZZ9.99  BLANK ZERO.
           03  D-TOT-MAINT-CHG PIC ZZZZZ9.99  BLANK ZERO.
           03  D-FEES          OCCURS 3 TIMES.
               05  D-FEEDESC   PIC X(17).
               05  D-FEEAMT    PIC ZZZZZ9.99  BLANK ZERO.

           03  D-1STPYAMT      PIC ZZZZZ9.99  BLANK ZERO.
           03  D-AMT-REG-PYAMT PIC ZZZZZ9.99  BLANK ZERO.
           03  D-NO-REG-PYAMT  PIC ZZZ        BLANK ZERO.
           03  D-LAST-PYAMT    PIC ZZZZZ9.99  BLANK ZERO.
           03  D-FIN-AMT       PIC ZZZZZ9.99  BLANK ZERO.
           03  D-FACE-AMT      PIC ZZZZZ9.99  BLANK ZERO.
           03  D-TOT-REPAY     PIC ZZZZZ9.99  BLANK ZERO.
           03  D-APR           PIC ZZZ.ZZZ    BLANK ZERO.

       01  DISPLAY-BUF2        REDEFINES DISPLAY-BUF1.
           03  D-270-300      OCCURS 4.
               05  D-1COMP     PIC 99        BLANK ZERO.
               05  D-1OUR      PIC X.
               05  D-1TERM     PIC ZZ9       BLANK ZERO.
               05  D-1COVR     PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM     PIC Z(3)9.99  BLANK ZERO.
           03  D-310-320      OCCURS 2.
               05  D-2COMP     PIC 99        BLANK ZERO.
               05  D-2OUR      PIC X.
               05  D-2EXP      PIC 9(6)      BLANK ZERO.
               05  D-2COVR     PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM     PIC Z(3)9.99  BLANK ZERO.

       01  DISPLAY-LIST1.
           03  FILLER             PIC X(40)  VALUE
           "010 011 012 020 021 022 030 031 032 040 ".
           03  FILLER             PIC X(40)  VALUE
           "050 051 060 061 070 080 090 100 CLOSING ".
           03  FILLER             PIC X(31)  VALUE
           "110 120P 120 121P 121 122P 122 ".
           03  FILLER             PIC X(32)  VALUE
           "131 141 140 150 160 170 171 180.".

       01  DISPLAY-LIST2.
           03  FILLER          PIC X(40)  VALUE
           "270 271 272 273 274 280 281 282 283 284 ".
           03  FILLER          PIC X(40)  VALUE
           "290 291 292 293 294 300 301 302 303 304 ".
           03  FILLER          PIC X(40)  VALUE
           "310 311 312 313 314 320 321 322 323 324.".

       01  DISPLAY-LIST3.
           03  FILLER             PIC X(24)  VALUE
           "190 200 201 210 211 220.".
       01  DISPLAY-BUF3.
           03  D-PRIOR-ACCTNO     PIC 9(6)       BLANK ZERO.
           03  D-PRIOR-LOAN-DATE  PIC 99/99/99   BLANK ZERO.
           03  D-PRIOR-POFFDATE   PIC 99/99/99   BLANK ZERO.
           03  D-PRIOR-PO-BAL     PIC ZZZZZ9.99  BLANK ZERO.
           03  D-PRIOR-PAYMENT    PIC ZZZZZ9.99  BLANK ZERO.
           03  D-REFIN-BAL        PIC ZZZZZ9.99  BLANK ZERO.

       COPY "LIBWI/LESCANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".

       01  GAINQY-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/GAINQY_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/GAINQY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/GAINQW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/GAINQY_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME GAINQ-BUF EXIT-PATHNAME.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GAINQY" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE GAINQ-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  GAINQ-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    GAINQ-COLUMNS + VDU-WIN-W-MOD
                   LINES   GAINQ-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GAINQY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.
      D    STOP MESS.

           MOVE "S  A200TITLE." TO VDU.
           MOVE "  ACCOUNT  INQUIRY  " TO VDUBUF.
           PERFORM SCREENIO.

      *    IF GAINQ-ARCHIVE-FG = "Y"
      *       PERFORM SETUP-ARCHIVE-PATHS
      *       IF IO-BAD
      *          GO TO END-ROUTINE.

           MOVE GAINQ-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO END-ROUTINE.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS.

           PERFORM DISPLAY-FIELDS.


       ENTER-AGAIN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "18" OR "1:"
              IF VDUSTATUS = "18"
                 MOVE "R" TO LESCAN-TYPE
              ELSE
                 MOVE "O" TO LESCAN-TYPE
              END-IF
              MOVE LN-OWNBR     TO LESCAN-BRNO
              MOVE LN-ACCTNO    TO LESCAN-ACCTNO
              MOVE LN-SSNO(1)    TO LESCAN-SSNO(1)
              MOVE LN-SSNO(2)    TO LESCAN-SSNO(2)
              MOVE LN-SSNO(3)    TO LESCAN-SSNO(3)
              MOVE LN-SSNO(4)    TO LESCAN-SSNO(4)
              MOVE LN-MAKERCD(1) TO LESCAN-MAKERCD(1)
              MOVE LN-MAKERCD(2) TO LESCAN-MAKERCD(2)
              MOVE LN-MAKERCD(3) TO LESCAN-MAKERCD(3)
              MOVE LN-MAKERCD(4) TO LESCAN-MAKERCD(4)
              MOVE LN-LNAMT      TO LESCAN-LNAMT
              MOVE "WI/LESCAN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH LESCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO ENTER-AGAIN.

      * SET FLAG TO PASS BACK TO AUDIQ1 TO GET NEXT ACCOUNT IN Q
           IF VDUSTATUS = "12"
              MOVE VDUSTATUS TO GAINQ-STAT
              GO TO END-ROUTINE.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           GO TO ENTER-AGAIN.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF1.
           PERFORM GET-SP.
           MOVE LN-CLASS TO LC-CODE.
           PERFORM GET-LC.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM GET-BW1.
           MOVE BW-LOANS TO HOLD-BW-LOANS.
           MOVE 2 TO ELE.

       DISP-NEXT-BW.
           MOVE LN-SSNO(ELE) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-SSNO(ELE)
              MOVE INSPECT-W TO D-NAME(ELE)
              MOVE LN-MAKERCD(ELE) TO D-CODE(ELE).
           IF ELE < 4
              ADD 1 TO ELE
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           IF HOLD-BW-LOANS > 1
              IF LN-POCD-OPEN
                 PERFORM FIND-OTHER-ACCOUNTS
              ELSE
                 PERFORM FIND-DUAL-ACCOUNT.

           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE D-COL-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC   TO D-COL-DESC.
           MOVE "SR"    TO CD-TYPE.
           MOVE LN-SRCD TO CD-CODE D-SRC-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-SRC-DESC.
           COMPUTE D-FEE-4 = LN-SERCHG - LN-STATE-WORKER - LN-POINTS.
           MOVE LN-STATE-WORKER TO D-FEE-8.
           MOVE LN-POINTS TO D-CLOSING.
           COMPUTE D-TOT-MAINT-CHG = LN-MAINTFEE * LN-ORGTERM.
           MOVE LN-INTCHG TO D-INT-CHGS.
           MOVE LN-FINCHG TO D-FIN-CHGS.
           PERFORM SETUP-FEES-FOR-DISPLAY.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE HOLD-FEEAMT(SUB) TO D-FEEAMT(SUB)
               MOVE HOLD-FEE-DESCRIPTION(SUB) TO D-FEEDESC(SUB)
           END-PERFORM.
           MOVE LN-1STPYAMT TO D-1STPYAMT.
           MOVE LN-NOREGPY TO D-NO-REG-PYAMT.
           MOVE LN-REGPYAMT TO D-AMT-REG-PYAMT.
           MOVE LN-LASTPYAMT TO D-LAST-PYAMT.
           PERFORM LOAN-CALCULATIONS.
           MOVE FINANCED-AMOUNT TO D-FIN-AMT.
           COMPUTE D-FACE-AMT = LN-LNAMT - (LN-MAINTFEE * LN-ORGTERM).
           MOVE TOTAL-REPAY TO D-TOT-REPAY.
           MOVE LN-APRATE TO D-APR.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 1 TO ELE.
       DISP-NEXT-IC.
           MOVE LN-INSCOMP(ELE) TO D-1COMP(ELE).
           MOVE LN-INSPREM(ELE) TO D-1PREM(ELE).
           MOVE LN-INSTRM(ELE) TO D-1TERM(ELE).
           MOVE LN-INSCOVR(ELE) TO D-1COVR(ELE).
           MOVE LN-INSOURS(ELE) TO D-1OUR(ELE).
           IF ELE < 4
              ADD 1 TO ELE
              GO TO DISP-NEXT-IC.

           MOVE 1 TO SUB.
           MOVE 5 TO ELE.
       DISP-NEXT-2IC.
           MOVE LN-INSCOMP(ELE) TO D-2COMP(SUB).
           MOVE LN-INSPREM(ELE) TO D-2PREM(SUB).
           MOVE LN-INSEXP-DATE(ELE) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-2EXP(SUB).
           MOVE LN-INSCOVR(ELE) TO D-2COVR(SUB).
           MOVE LN-INSOURS(ELE) TO D-2OUR(SUB).
           IF ELE < 6
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-2IC.

           MOVE 1 TO ELE.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF (LN-POCD-OPEN)
              IF LN-PRBAL NOT = 0
                  PERFORM DISPLAY-PRIOR-RN
              ELSE
                 PERFORM DISPLAY-PAIDOUT.

      ************************************
       DISPLAY-PAIDOUT SECTION.
      ************************************
           MOVE " " TO DISPLAY-BUF3.
           PERFORM GET-RESTRICT-REC.
           MOVE DISPLAY-BUF3 TO WR-BUF.
           MOVE DISPLAY-LIST3 TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************************************
       GET-RESTRICT-REC SECTION.
      ************************************************
           PERFORM OPEN-LE1-FILE.

           MOVE LE-PATH-OWNBR TO LE-BRNO
                                 QLE1-WBEG-BRNO
                                 QLE1-WEND-BRNO.
           MOVE GAINQ-ACCTNO  TO LE-ACCTNO
                                     QLE1-WBEG-ACCTNO
                                     QLE1-WEND-ACCTNO.
           MOVE "R"           TO LE-TYPE
                                 QLE1-WBEG-TYPE
                                 QLE1-WEND-TYPE.
           MOVE "P"           TO LE-FLAG
                                 QLE1-WBEG-FLAG
                                 QLE1-WEND-FLAG.
           MOVE 0             TO LE-SSNO
                                 QLE1-WBEG-SSNO.
           MOVE 0             TO LE-OSACCTNO
                                 QLE1-WBEG-OSACCTNO.
           MOVE ALL "9"       TO QLE1-WEND-SSNO
                                 QLE1-WEND-OSACCTNO.

           PERFORM START-LE1-FILE.
       GET-RESTRICT-REC-NEXT.
           PERFORM READ-LE1-FILE-NEXT.
           IF IO-FG = 9 OR (LE-ACCTNO NOT = GAINQ-ACCTNO) OR
                             (LE-BRNO NOT = LE-PATH-OWNBR)
              GO TO GET-RESTRICT-REC-EXIT.

           IF LE-TYPE NOT = "R"
              GO TO GET-RESTRICT-REC-NEXT.
           IF LE-FLAG NOT = "P"
              GO TO GET-RESTRICT-REC-EXIT.

           IF LE-R-SALEFIN = "Y"
              GO TO GET-RESTRICT-REC-NEXT.

           MOVE LE-OSACCTNO   TO D-PRIOR-ACCTNO.
           MOVE LE-R-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-PRIOR-LOAN-DATE.
           MOVE LE-R-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-PRIOR-POFFDATE.

           MOVE LE-OSACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.
           IF IO-FG = 0
              MOVE LN-AMT-PAID-LAST TO D-PRIOR-PAYMENT.

           GO TO GET-RESTRICT-REC-NEXT.

       GET-RESTRICT-REC-EXIT.
           PERFORM CLOSE-LE1-FILE.

           MOVE GAINQ-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.

      ************************************
       DISPLAY-PRIOR-RN SECTION.
      ************************************
           MOVE " " TO DISPLAY-BUF3.
           MOVE LN-PRLNNO TO D-PRIOR-ACCTNO.
           MOVE LN-PRBAL TO D-PRIOR-PO-BAL.
           COMPUTE D-REFIN-BAL = (LN-PRBAL + LN-PRINTDUE) -
                                    (LN-PRFINCHGRB + LN-PRINSRB).
           MOVE LN-PRLNNO TO LN-ACCTNO.
           IF LN-BR-TFR-FG = "Y"
              PERFORM GET-PRIOR-TRANSFERRED
           ELSE
              PERFORM GET-LN.
           IF IO-FG = 0
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PRIOR-LOAN-DATE
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PRIOR-POFFDATE
              MOVE DISPLAY-BUF3 TO WR-BUF
              MOVE DISPLAY-LIST3 TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE GAINQ-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.

      ************************************
       SETUP-FEES-FOR-DISPLAY SECTION.
      ************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                MOVE SPACES TO HOLD-FEECODE(SUB)
                               HOLD-FEEDESC(SUB)
                MOVE " "    TO HOLD-FEECOLON(SUB)
                MOVE ZEROS  TO HOLD-FEEAMT(SUB)
           END-PERFORM.

           PERFORM FIND-FEES VARYING SUB FROM 1 BY 1 UNTIL SUB > 10.

           IF HOLD-FEEAMT(4) NOT = 0
              ADD HOLD-FEEAMT(4)   TO HOLD-FEEAMT(3)
              MOVE HOLD-FEECODE(4) TO HOLD-FEECODE(3)
              MOVE HOLD-FEEDESC(4) TO HOLD-FEEDESC(3).

       SETUP-FEES-FOR-DISPLAY-EXIT.
           EXIT.

      ****************************
       FIND-FEES SECTION.
      ****************************
           MOVE 1 TO SUB2.
       FIND-FEE-LOOP.
           IF LN-FEEAMT(SUB) = 0
              GO TO  FIND-FEES-EXIT.

           IF HOLD-FEEAMT(SUB2) = 0 OR
              (HOLD-FEEAMT(SUB2) > 0 AND
                              LN-FEECODE(SUB) = HOLD-FEECODE(SUB2))
              ADD LN-FEEAMT(SUB)    TO HOLD-FEEAMT(SUB2)
              MOVE LN-FEECODE(SUB)  TO HOLD-FEECODE(SUB2)
              PERFORM GET-FEECD
              MOVE CDB-DESC       TO HOLD-FEEDESC(SUB2)
              MOVE ":"            TO HOLD-FEECOLON(SUB2)
              GO TO FIND-FEES-EXIT.

           IF SUB2 < 3
              ADD 1 TO SUB2
              GO TO FIND-FEE-LOOP.

           ADD LN-FEEAMT(SUB)       TO HOLD-FEEAMT(4).
           MOVE LN-FEECODE(SUB)     TO HOLD-FEECODE(4).
           MOVE "OTHER FEES      "  TO HOLD-FEEDESC(4).
           MOVE ":"    TO HOLD-FEECOLON(4).

       FIND-FEES-EXIT.
           EXIT.

      *****************************************
       GET-PRIOR-TRANSFERRED SECTION.
      *****************************************

           MOVE " " TO LN-PATH-OVERRIDE.
           PERFORM LOAD-LN1-FILE.
           MOVE LN-PATH TO SAVE-LN-PATH.

           MOVE LN-ORGBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO EXIT-GET-PRIOR-TRANSFERRED.
           PERFORM CLOSE-BR-FILE.

           IF BR-STATE NOT = "GA"
              MOVE 9 TO IO-FG
              GO TO EXIT-GET-PRIOR-TRANSFERRED.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"         TO LN-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

       EXIT-GET-PRIOR-TRANSFERRED.
           MOVE SAVE-LN-PATH TO LN-PATH.
           MOVE " " TO LN-PATH-OVERRIDE.

      ******************************************************************
       FIND-DUAL-ACCOUNT SECTION.
      ******************************************************************
           MOVE SPACES TO D-OTHER-LOAN-LINE.
           PERFORM OPEN-LE1-FILE.

           MOVE SPACES       TO LE1-KEY.

           MOVE LE-PATH-OWNBR TO LE-BRNO
                                 QLE1-WBEG-BRNO
                                 QLE1-WEND-BRNO.
           MOVE GAINQ-ACCTNO  TO LE-ACCTNO
                                     QLE1-WBEG-ACCTNO
                                     QLE1-WEND-ACCTNO.
           MOVE "R"           TO LE-TYPE
                                 QLE1-WBEG-TYPE
                                 QLE1-WEND-TYPE.
           MOVE "D"           TO LE-FLAG
                                 QLE1-WBEG-FLAG
                                 QLE1-WEND-FLAG.
           MOVE 0             TO LE-SSNO
                                 QLE1-WBEG-SSNO.
           MOVE 0             TO LE-OSACCTNO
                                 QLE1-WBEG-OSACCTNO.
           MOVE ALL "9"       TO QLE1-WEND-SSNO
                                 QLE1-WEND-OSACCTNO.

           PERFORM START-LE1-FILE.
           MOVE 1 TO SUB2.
       FIND-DUAL-ACCOUNT-NEXT.
           PERFORM READ-LE1-FILE-NEXT.
           IF IO-FG = 9 OR (LE-ACCTNO NOT = GAINQ-ACCTNO) OR
                            (LE-BRNO NOT = LE-PATH-OWNBR)
              GO TO FIND-DUAL-ACCOUNT-EXIT.

           IF LE-TYPE NOT = "R"
              GO TO FIND-DUAL-ACCOUNT-NEXT.
           IF LE-FLAG NOT = "D"
              GO TO FIND-DUAL-ACCOUNT-EXIT.

           IF LE-R-SALEFIN = "Y" 
              GO TO FIND-DUAL-ACCOUNT-NEXT.

           MOVE LE-OSACCTNO TO D-OTHER-LOAN2.
           MOVE LE-R-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-OTHER-LOANDATE2.
           MOVE LE-R-BALANCE  TO D-OTHER-CURBAL2.
       FIND-DUAL-ACCOUNT-EXIT.
           PERFORM CLOSE-LE1-FILE.

      ******************************************************************
      * FIND OTHER ACCOUNTS FOR THIS BORROWER AND DISPLAY 3 OF THEM
      ******************************************************************
       FIND-OTHER-ACCOUNTS SECTION.

           MOVE LN-REC     TO HOLD-LN-REC.
           MOVE LN-SSNO(1) TO HOLD-LN-SSNO.
           MOVE LN-ACCTNO  TO HOLD-LN-ACCTNO.
           MOVE LN-CLASS   TO HOLD-LN-CLASS.
           MOVE 0 TO OTHER-ACCTNO(1) OTHER-ACCTNO(2) OTHER-ACCTNO(3).

           MOVE 1 TO SUB2.
      *    PERFORM FIND-ALL-LOANS VARYING SUB FROM 4 BY 1 UNTIL SUB > 7.
           PERFORM FIND-ALL-LOANS VARYING SUB FROM 4 BY 1 UNTIL SUB > 5.

      * SETUP DETAIL LINE, 1 ACCOUNT #, LOAN DATE, AND BALANCE
           IF OTHER-ACCTNO(1) NOT = 0
              MOVE OTHER-ACCTNO(1) TO LN-ACCTNO
              PERFORM OPEN-LN1-FILE
              PERFORM READ-LN1-FILE
              PERFORM CLOSE-LN1-FILE
              IF IO-FG NOT = 0
                 MOVE SPACES TO D-OTHER-LOAN-LINE2
                 GO TO FIND-OTHER-ACCOUNTS-EXIT
              END-IF
              MOVE LN-ACCTNO   TO D-OTHER-LOAN2
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-OTHER-LOANDATE2
              MOVE LN-CURBAL   TO D-OTHER-CURBAL2
              GO TO FIND-OTHER-ACCOUNTS-EXIT.

      * SETUP DETAIL LINE, UP TO 3 ACCTNOS COMMA DELIMITED
           IF OTHER-ACCTNO(1) NOT = 0
              MOVE OTHER-ACCTNO(1) TO D-OTHER-LOAN-1
              IF OTHER-ACCTNO(2) NOT = 0
                 MOVE ","           TO D-OTHER-LOAN-SLASH2
                 MOVE OTHER-ACCTNO(2) TO D-OTHER-LOAN-2
                 IF OTHER-ACCTNO(3) NOT = 0
                    MOVE ","           TO D-OTHER-LOAN-SLASH3
                    MOVE OTHER-ACCTNO(3) TO D-OTHER-LOAN-3.


       FIND-OTHER-ACCOUNTS-EXIT.
           MOVE HOLD-LN-REC TO LN-REC.

      ******************************************************************
       FIND-ALL-LOANS SECTION.
      ******************************************************************
           PERFORM OPEN-LN4-FILE.

           MOVE HOLD-LN-SSNO TO LN-SSNO(SUB - 3).
           IF SUB = 4
              MOVE LN-PATH-OWNBR   TO LN-OWNBR
                                      QLN4-WBEG-OWNBR
                                      QLN4-WEND-OWNBR
              MOVE LN-SSNO(SUB - 3) TO QLN4-WBEG-SSNO1
                                      QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
           IF SUB = 5
              MOVE LN-PATH-OWNBR   TO LN-OWNBR
                                      QLN5-WBEG-OWNBR
                                      QLN5-WEND-OWNBR
              MOVE LN-SSNO(SUB - 3) TO QLN5-WBEG-SSNO2
                                      QLN5-WEND-SSNO2
              PERFORM START-LN5-FILE.
      *    ELSE
      *    IF SUB = 6
      *       MOVE LN-PATH-OWNBR   TO LN-OWNBR
      *                               QLN6-WBEG-OWNBR
      *                               QLN6-WEND-OWNBR
      *       MOVE LN-SSNO(SUB - 3) TO QLN6-WBEG-SSNO3
      *                               QLN6-WEND-SSNO3
      *       PERFORM START-LN6-FILE
      *    ELSE
      *       MOVE LN-PATH-OWNBR   TO LN-OWNBR
      *                               QLN7-WBEG-OWNBR
      *                               QLN7-WEND-OWNBR
      *       MOVE LN-SSNO(SUB - 3) TO QLN7-WBEG-SSNO4
      *                               QLN7-WEND-SSNO4
      *       PERFORM START-LN7-FILE.
       FIND-NEXT-LOAN.
           EVALUATE SUB
              WHEN 4 PERFORM READ-LN4-FILE-NEXT
              WHEN 5 PERFORM READ-LN5-FILE-NEXT
      *       WHEN 6 PERFORM READ-LN6-FILE-NEXT
      *       WHEN 7 PERFORM READ-LN7-FILE-NEXT
           END-EVALUATE.

      * QUIT IF NO OTHER LOANS FOR BORROWER

           IF IO-FG NOT = 0 OR LN-SSNO(SUB - 3) > HOLD-LN-SSNO
                             OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO FIND-ALL-LOANS-EXIT.

      * SKIP IF NOT AN OPEN LOAN OR NOT A SMALL LOAN

           IF NOT (LN-POCD-OPEN AND LN-SMALLOAN = "Y")
              GO TO FIND-NEXT-LOAN.

      * SKIP CURRENT ACCOUNT NUMBER

           IF LN-ACCTNO = HOLD-LN-ACCTNO
              GO TO FIND-NEXT-LOAN.

           MOVE LN-ACCTNO TO OTHER-ACCTNO(SUB2).
           IF SUB2 < 3
              ADD 1 TO SUB2
              GO TO FIND-NEXT-LOAN.

       FIND-ALL-LOANS-EXIT.
           PERFORM CLOSE-LN4-FILE.

       FIND-ALL-LOANS-EXIT2.
           EXIT.

      *****************************************************************
      *  SET UP PATH IF LOAN IS FLAGGED AS BEING IN ARCHIVE DIRECTORY
      *****************************************************************
      *SETUP-ARCHIVE-PATHS SECTION.

      *    MOVE EXT-FILPATH          TO LN-ARCHIVE-PATH
      *                                 LE-ARCHIVE-PATH.
      *    MOVE GP-ARCHIVE-DIRECTORY TO LN-ARCHIVE-DIRECTORY
      *                                 LE-ARCHIVE-DIRECTORY.

      *    MOVE "Y" TO LE-PATH-OVERRIDE
      *                LN-PATH-OVERRIDE.
      *    MOVE LE-ARCHIVE-PATH      TO LE-PATH.
      *    MOVE LN-ARCHIVE-PATH      TO ACCESS-BUF LN-PATH.
      *    PERFORM ACCESS-CALL.
      *    IF STAT-BAD
      *       MOVE "INVALID ARCHIVE PATH!" TO MESS
      *       PERFORM SEND-MESS
      *       MOVE 9 TO IO-FG.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".


      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON GAINQY-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY GAINQY-SCREEN UPON GAINQY-WINDOW-HANDLE.
           MOVE GAINQY-FORM-FIELDS TO WR-BUF.
           MOVE GAINQY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE GAINQY-HELP--FILE TO HELP-LINK-FILE.
           MOVE GAINQY-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/GAINQY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".

       GET-FEECD.
           MOVE "GC"            TO CDB-TYPE.
           MOVE LN-OWNBR        TO CDB-BRANCH.
           MOVE LN-FEECODE(SUB) TO CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE "FEE NOT FOUND" TO CDB-DESC.
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       GET-SP.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              MOVE "STATE PARA DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
           PERFORM CLOSE-SP1-FILE.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 9
              MOVE " INVALID CODE" TO CD-DESC.
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE " INVALID DEALER" TO DL-SHORT.
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       ADD-IT2.
           PERFORM ADD-IT.
           MOVE "/" TO ADD-ON.
           PERFORM ADD-IT.
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW.
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-MEMO.
           MOVE "S  A060M1." TO VDU.
           MOVE "MEMO: " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300M2." TO VDU.
           MOVE LN-MESSAGE TO VDUBUF.
           PERFORM SCREENIO.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.


      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LE1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLE1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GAINQY-SCREEN.
           DESTROY GAINQY-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
