       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AOLIST.
       DATE-WRITTEN.  02/13/95.
      *********************************************************
      *    DIR :  LS
      *    DESC:  ADDON
      *
      * REV :
      *  JFF 102095 ADDED BORROWER LOAN SCAN
      *  MG  991007 CHANGED CHANGED "PAYMENT SCHEDULE FILE LISTING"
      *             TO "ADDON FILE LISTING".       PL#:247
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *
      * BAH 170425 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 190104 GLOBAL LTIFILE
      *  CS 190104 FIXED PRINT OF A1-ACCTNO, WAS 6 NUMBER, 2 SEQ
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/AOLIST.CBL S35 02/21/24-12:03:45 >".

      *********************************************************
      *     F I L E   I F N
      *********************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

      *********************************************************
      *   L I N E   W O R K E R S
      *********************************************************
       01  LN-FEED              PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
       01  PAGE-LINE-NO         PIC 99       VALUE 10.

      *********************************************************
      *   M I S C .   W O R K E R S
      *********************************************************
       01  END-PERFORM-PFL      PIC X        VALUE "N".
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  HOLD-IO-FG           PIC 9        VALUE 0.
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".

       01  SUB-1                PIC 99       VALUE ZERO.
       01  SUB-2                PIC 99       VALUE ZERO.

       01  WS-ORIG-ANTIC        PIC S9(6)V99.
       01  WS-UNEARNED          PIC S9(6)V99.

       01  LOAN-FG              PIC X(3)      VALUE "   ".
           88  LOAN-BEG                       VALUE "BEG".
           88  LOAN-END                       VALUE "END".
      *********************************************************
      *     S C R E E N   F I E L D S
      *********************************************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-LN          PIC 9(6).
           03  END-LN          PIC 9(6).

       01  OK                   PIC X        VALUE " ".


      *********************************************************
      *     P R I N T   B U F F E R S
      *********************************************************

       01  HEAD1.
           03  H-DATE      PIC X(8)      VALUE " ".
           03  FILLER      PIC X         VALUE " ".
           03  H-HOUR      PIC 9(2)      VALUE ZERO.
           03  FILLER      PIC X         VALUE ":".
           03  H-MIN       PIC 9(2)      VALUE ZERO.
           03  FILLER      PIC X(34)     VALUE " ".
           03  H-CONAME    PIC X(40)     VALUE " ".
           03  FILLER      PIC X(35)     VALUE " ".
           03  FILLER      PIC X(6)      VALUE "PAGE: ".
           03  H-PAGE-NO   PIC 999       VALUE ZERO.

      *--------------------------------------------------------

       01  HEAD2.
           03  FILLER      PIC X(9)      VALUE "USER ID:".
           03  H-USERID    PIC X(12)     VALUE SPACES.
           03  FILLER      PIC X(39)     VALUE SPACES.
           03  FILLER      PIC X(18)     VALUE
                                    "ADDON FILE LISTING".
           03  FILLER      PIC X(54)     VALUE SPACES.

      *--------------------------------------------------------
       01  SUB-HEAD1.
           03  FILLER      PIC X(13)    VALUE "LOAN#-SEQ-SEQ".
           03  FILLER      PIC X(2)     VALUE SPACES.
           03  FILLER      PIC X(14)    VALUE "ORG-1STPY-DATE".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(14)    VALUE "PRIOR # DEFMNT".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(8)     VALUE "INS-TYPE".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(7)     VALUE "TR-CODE".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(15)    VALUE "REGPAY-INCREASE".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(17)    VALUE "LOAN-AMT-INCREASE".
           03  FILLER      PIC X(3)     VALUE SPACES.
           03  FILLER      PIC X(16)    VALUE "FIN-CHG-INCREASE".
           03  FILLER      PIC X(7)     VALUE SPACES.

      *--------------------------------------------------------
       01  SUB-HEAD2.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  FILLER      PIC X(48)    VALUE ALL "- ".
           03  FILLER      PIC X(22)    VALUE "INSURANCE INFORMATION ".
           03  FILLER      PIC X(47)    VALUE ALL "- ".

      *--------------------------------------------------------
       01  SUB-HEAD3.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  FILLER      PIC X(2)      VALUE "CO".
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE "OURS".
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  FILLER      PIC X(8)      VALUE "EFF-DATE".
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  FILLER      PIC X(8)      VALUE "EXP-DATE".
           03  FILLER      PIC X(6)      VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE "TERM".
           03  FILLER      PIC X(8)      VALUE SPACES.
           03  FILLER      PIC X(8)      VALUE "COVERAGE".
           03  FILLER      PIC X(7)      VALUE SPACES.
           03  FILLER      PIC X(7)      VALUE "PREMIUM".
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  FILLER      PIC X(10)     VALUE "COMMISSION".
           03  FILLER      PIC X(6)      VALUE SPACES.
           03  FILLER      PIC X(3)      VALUE "REB".
           03  FILLER      PIC X(15)     VALUE SPACES.

      *--------------------------------------------------------
       01  SUB-HEAD4.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  FILLER      PIC X(51)     VALUE ALL "- ".
           03  FILLER      PIC X(16)     VALUE "FEE INFORMATION ".
           03  FILLER      PIC X(50)     VALUE ALL "- ".

      *--------------------------------------------------------
       01  SUB-HEAD5.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  FILLER      PIC X(2)      VALUE "CD".
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  FILLER      PIC X(11)     VALUE "DESCRIPTION".
           03  FILLER      PIC X(13)     VALUE SPACES.
           03  FILLER      PIC X(6)      VALUE "AMOUNT".
           03  FILLER      PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(2)      VALUE "CD".
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  FILLER      PIC X(11)     VALUE "DESCRIPTION".
           03  FILLER      PIC X(13)     VALUE SPACES.
           03  FILLER      PIC X(6)      VALUE "AMOUNT".
           03  FILLER      PIC X(23)     VALUE SPACES.

      *--------------------------------------------------------
       01  SUB-HEAD6.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  FILLER      PIC X(48)     VALUE ALL "- ".
           03  FILLER      PIC X(21)     VALUE "EARNINGS INFORMATION ".
           03  FILLER      PIC X(48)     VALUE ALL "- ".

      *--------------------------------------------------------

       01  SUB-HEAD7.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  FILLER      PIC X(8)      VALUE "ANT-TERM".
           03  FILLER      PIC X(2)      VALUE SPACES.
           03  FILLER      PIC X(10)     VALUE "START-DATE".
           03  FILLER      PIC X(2)      VALUE SPACES.
           03  FILLER      PIC X(3)      VALUE "REB".
           03  FILLER      PIC X(2)      VALUE SPACES.
           03  FILLER      PIC X(8)      VALUE "WORK-AMT".
           03  FILLER      PIC X(3)      VALUE SPACES.
           03  FILLER      PIC X(10)     VALUE "ORIG ANTIC".
           03  FILLER      PIC X(5)      VALUE "  -  ".
           03  FILLER      PIC X(9)      VALUE "ADJ ANTIC".
           03  FILLER      PIC X(5)      VALUE "  =  ".
           03  FILLER      PIC X(9)      VALUE "NET ANTIC".
           03  FILLER      PIC X(6)      VALUE "  -   ".
           03  FILLER      PIC X(8)      VALUE "EARNINGS".
           03  FILLER      PIC X(6)      VALUE "  =   ".
           03  FILLER      PIC X(8)      VALUE "UNEARNED".
           03  FILLER      PIC X(2)      VALUE SPACES.
           03  FILLER      PIC X(10)     VALUE "...UPFRONT".

      *--------------------------------------------------------

       01  ACTION1.
      *    03  A1-ACCTNO   PIC 999999/99 VALUE ZERO.
           03  A1-ACCTNO   PIC 99999999 VALUE ZERO.
      *    03  FILLER      PIC X         VALUE "-".
      *    03  A1-LNSEQ    PIC 99        VALUE ZERO.
           03  FILLER      PIC X         VALUE "-".
           03  A1-ADDSEQ   PIC 999       BLANK WHEN ZERO.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A1-ORG-1PY  PIC 99/99/99  BLANK WHEN ZERO.
           03  FILLER      PIC X(12)     VALUE SPACES.
           03  A1-PR-NUM-D PIC 99-       BLANK WHEN ZERO.
           03  FILLER      PIC X(12)     VALUE SPACES.
           03  A1-INS-TYPE PIC X(2)      VALUE SPACES.
           03  FILLER      PIC X(8)      VALUE SPACES.
           03  A1-TR-CODE  PIC X(2)      VALUE SPACES.
           03  FILLER      PIC X(8)      VALUE SPACES.
           03  A1-REGPY-IN PIC ZZZ,ZZ9.99-   BLANK WHEN ZERO.
           03  FILLER      PIC X(8)      VALUE SPACES.
           03  A1-LNAMT-IN PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  FILLER      PIC X(9)      VALUE SPACES.
           03  A1-FINCHG   PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  FILLER      PIC X(10)     VALUE SPACES.

      *--------------------------------------------------------

       01  ACTION2.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  A2-CO       PIC 9(2)      BLANK WHEN ZERO.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A2-OURS     PIC X         VALUE SPACES.
           03  FILLER      PIC X(7)      VALUE SPACES.
           03  A2-EFF-DATE PIC 99/99/99  BLANK WHEN ZERO.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A2-EXP-DATE PIC 99/99/99  BLANK WHEN ZERO.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A2-TERM     PIC Z(6)      BLANK WHEN ZERO.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A2-COVERAGE PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A2-PREMIUM  PIC Z,ZZ9.99-       BLANK WHEN ZERO.
           03  FILLER      PIC X(6)      VALUE SPACES.
           03  A2-COMMISN  PIC Z,ZZ9.99-       BLANK WHEN ZERO.
           03  FILLER      PIC X(6)      VALUE SPACES.
           03  A2-REBATE   PIC X         VALUE SPACES.
           03  FILLER      PIC X(18)     VALUE SPACES.

      *--------------------------------------------------------

       01  ACTION3.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  A3-CD-1     PIC X(2)      VALUE SPACES.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A3-DESC-1   PIC X(17)     VALUE SPACES.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A3-AMOUNT-1 PIC Z,ZZ9.99        BLANK WHEN ZERO.
           03  FILLER      PIC X(20)     VALUE SPACES.
           03  A3-CD-2     PIC X(2)      VALUE SPACES.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A3-DESC-2   PIC X(17)     VALUE SPACES.
           03  FILLER      PIC X(5)      VALUE SPACES.
           03  A3-AMOUNT-2 PIC Z,ZZ9.99        BLANK WHEN ZERO.
           03  FILLER      PIC X(23)     VALUE SPACES.

      *--------------------------------------------------------

       01  ACTION4.
           03  FILLER      PIC X(17)     VALUE SPACES.
           03  A4-ANT-TERM PIC ZZ9       BLANK WHEN ZERO.
           03  FILLER      PIC X(6)      VALUE SPACES.
           03  A4-START-DT PIC 99/99/99  BLANK WHEN ZERO.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A4-REBATED  PIC X         VALUE SPACES.
           03  FILLER      PIC X(2)      VALUE SPACES.
           03  A4-WORK-AMT PIC ZZ,ZZ9.99-      BLANK WHEN ZERO.
           03  FILLER      PIC X(2)      VALUE SPACES.
           03  A4-ORIG-ANT PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  FILLER      PIC X(3)      VALUE " - ".
           03  A4-ADJ-ANT  PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  FILLER      PIC X(3)      VALUE " = ".
           03  A4-NET-ANT  PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  FILLER      PIC X(3)      VALUE " - ".
           03  A4-EARNINGS PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  FILLER      PIC X(3)      VALUE " = ".
           03  A4-UNEARNED PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.
           03  A4-ALP      REDEFINES A4-UNEARNED
                           PIC X(11).
           03  FILLER      PIC X         VALUE SPACES.
           03  A4-UPFRONT  PIC ZZZ,ZZ9.99-     BLANK WHEN ZERO.

      *--------------------------------------------------------
       01  DETAIL-LINE          PIC X(132) VALUE " ".

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(22).
           03  RANGE-SELECTION  PIC X(16).

      *--------------------------------------------------------
       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  AOLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/AOLIST_DEF.CPY".
       COPY "LIBLS/AOLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/AOLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *********************************************************
      *
      *              M A I N   M O D U L E :
      *
      *********************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO  FORM-PATH.
           MOVE "AOLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS AOLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.

           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE BEG-LN          TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
           MOVE END-LN          TO QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.

           PERFORM PROCESS-FILE-LISTING UNTIL END-PERFORM-PFL = "Y"

           GO TO END-ROUTINE.

       PROCESS-FILE-LISTING.
           PERFORM ACCESS-FILES.

           IF (HOLD-IO-FG = 9) OR (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
               (LTI-ACCTNO > END-LN)
              MOVE "Y" TO END-PERFORM-PFL
           ELSE
              IF (LN-ADDON-FG = "Y") 
                 PERFORM DETAIL-LINE-PROCESS.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      *********************************************************
       ACCESS-FILES SECTION.
      *********************************************************

      * HOLD-IO-FG:  THIS IS TO HOLD THE THE IO-FG ON A
      *              READ FOR THE LP-REC SO IT CAN BE CHECKED
      *              WHEN IN PROCESS-FILE-LISTING.

       ACCESS-NEXT-LT.
           PERFORM READ-LTI1-FILE-NEXT.
           MOVE IO-FG TO HOLD-IO-FG.

           IF (IO-FG = 9) OR (LTI-BRNO NOT = LTI-PATH-OWNBR)
              OR  (LTI-ACCTNO > END-LN)
              GO TO END-ACCESS-FILES
           ELSE
              IF (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
                  (LTI-ACCTNO < BEG-LN)
                 GO TO ACCESS-NEXT-LT
              ELSE
                 PERFORM NEXT-LN.

           GO TO END-ACCESS-FILES.

       NEXT-LN.
           MOVE LTI-PATH-OWNBR TO LN-OWNBR.
           MOVE LTI-ACCTNO     TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

       END-ACCESS-FILES.
           EXIT.

      *********************************************************
       INITIALIZATION SECTION.
      *********************************************************
           MOVE " " TO ERRCD.

       GETPR.
           MOVE AOLIST-QUEUE-POS TO Q-POS.
           MOVE AOLIST-COPIES-POS TO C-POS.
           MOVE AOLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-INIT.

      *--------------------------------------------------------
       LN-01.
           MOVE "ENNN060LOANNO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "16" GO TO ALL-LN.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO LOAN-FG
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
              GO TO LN-01.
           IF VDUSTATUS NOT = "00" GO TO LN-01.
           MOVE VDUNUM0 TO BEG-LN.

      *--------------------------------------------------------
       LN-02.
           MOVE "ENNN060LOANNO2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LN-01.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS = "1<"
              MOVE "END" TO LOAN-FG
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
              GO TO LN-02.
           IF VDUSTATUS NOT = "00" GO TO LN-02.
           MOVE VDUNUM0 TO END-LN.
           IF BEG-LN > END-LN
              MOVE "INVALID LOAN NUMBER RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO LN-01.
           GO TO OK-YN.

      *--------------------------------------------------------
       ALL-LN.
           MOVE "SNNN060LOANNO1." TO VDU.
           MOVE 0       TO  BEG-LN VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060LOANNO2." TO VDU.
           MOVE 999999  TO  END-LN VDUNUM0.
           PERFORM SCREENIO.

      *--------------------------------------------------------
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LN-02.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           MOVE VDUBUF TO OK.
           IF OK = "N" GO TO LN-01.
           IF OK NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-CONAME   TO  H-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO  H-DATE.
           MOVE EXT-CONAME-USER-ID  TO  H-USERID.

           GO TO EXIT-INITIALIZE.

       END-INIT.
           MOVE 1 TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************************
       DETAIL-LINE-PROCESS SECTION.
      *********************************************************
           ADD  1  TO  PAGE-LINE-NO.
           MOVE 1  TO  LN-FEED.
           MOVE 1  TO  SUB-1.
           MOVE 1  TO  SUB-2.

           IF PAGE-LINE-NO > 4
              MOVE 1  TO  PAGE-LINE-NO
              PERFORM HEAD-PAGE.

           PERFORM FILL-ACTION-LINES.
           PERFORM WRITE-ACTION-LINES.

           GO TO EXIT-DETAIL-LINE-PROCESS.

       FILL-ACTION-LINES.
           MOVE LTI-ACCTNO            TO A1-ACCTNO.
           MOVE LTI-SEQNO             TO A1-ADDSEQ.
           MOVE LTI-ORIG-1STPYDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO A1-ORG-1PY.
           MOVE LTI-INS-TYPE          TO A1-INS-TYPE.
           MOVE LTI-INS-PAYMNT-INCR   TO A1-REGPY-IN.
           MOVE LTI-PRIOR-NODEF       TO A1-PR-NUM-D.
           MOVE LTI-TRCD              TO A1-TR-CODE.
           MOVE LTI-INS-TOT-ADDON     TO A1-LNAMT-IN.
           MOVE LTI-ANTICERN(3)       TO A1-FINCHG.
           MOVE LTI-INSCOMP           TO A2-CO.
           MOVE LTI-INSOURS           TO A2-OURS.
           MOVE LTI-INSEFF-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO A2-EFF-DATE.
           MOVE LTI-INSEXP-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO A2-EXP-DATE.
           MOVE LTI-INSEFF-DATE TO NUM-DATE.
           MOVE LTI-INSEXP-DATE TO SYS-DATE.
           PERFORM TIM360.
           MOVE ELAPSED-MONTHS        TO A2-TERM.
           MOVE LTI-INSCOVR           TO A2-COVERAGE.
           MOVE LTI-INSPREM           TO A2-PREMIUM.
           MOVE LTI-INSCOMM           TO A2-COMMISN.
           MOVE LTI-INS-REBATE        TO A2-REBATE.
           MOVE LTI-INSFEECD(1)       TO A3-CD-1 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC              TO A3-DESC-1.
           MOVE LTI-INSFEE(1)         TO A3-AMOUNT-1.
           MOVE LTI-INSFEECD(2)       TO A3-CD-2 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC              TO A3-DESC-2.
           MOVE LTI-INSFEE(2)         TO A3-AMOUNT-2.
           MOVE LTI-ANTICTERM         TO A4-ANT-TERM.
           MOVE LTI-ANTIC-START-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO A4-START-DT.
           MOVE LTI-INT-REBATE        TO A4-REBATED.
           MOVE LTI-INT-LAST-EARNED   TO A4-WORK-AMT.
           PERFORM DISPLAY-FIELDS-COMPONENTS.
           MOVE WS-ORIG-ANTIC         TO A4-ORIG-ANT.
           MOVE LTI-ANTICADJ(1)       TO A4-ADJ-ANT.
           MOVE LTI-ANTICERN(1)       TO A4-NET-ANT.
           MOVE LTI-ACCERN(1)         TO A4-EARNINGS.
           MOVE LTI-UFERN(1)          TO A4-UPFRONT.
           INSPECT A1-ACCTNO REPLACING FIRST "/" BY "-".

       WRITE-ACTION-LINES.
           MOVE 1         TO  LN-FEED.

           MOVE SUB-HEAD1 TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION1   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SUB-HEAD2 TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SUB-HEAD3 TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION2   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SUB-HEAD4 TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SUB-HEAD5 TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION3   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SUB-HEAD6 TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SUB-HEAD7 TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION4   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-"   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-ACTION-LINES.

      *-----------------------------------------------------------
       CLEAR-ACTION-LINES.
           MOVE ZEROES  TO   A1-ACCTNO.
           MOVE 0   TO A1-ORG-1PY.
           MOVE " " TO A1-INS-TYPE.
           MOVE 0   TO A1-REGPY-IN.
           MOVE 0   TO A1-PR-NUM-D.
           MOVE " " TO A1-TR-CODE.
           MOVE 0   TO A1-LNAMT-IN.
           MOVE 0   TO A1-FINCHG.
           MOVE " " TO A2-CO.
           MOVE " " TO A2-OURS.
           MOVE 0   TO A2-EFF-DATE.
           MOVE 0   TO A2-EXP-DATE.
           MOVE 0   TO A2-TERM.
           MOVE 0   TO A2-COVERAGE.
           MOVE 0   TO A2-PREMIUM.
           MOVE 0   TO A2-COMMISN.
           MOVE " " TO A2-REBATE.
           MOVE " " TO A3-CD-1 CDB-CODE.
           MOVE " " TO A3-DESC-1.
           MOVE 0   TO A3-AMOUNT-1.
           MOVE " " TO A3-CD-2 CDB-CODE.
           MOVE " " TO A3-DESC-2.
           MOVE 0   TO A3-AMOUNT-2.
           MOVE 0   TO A4-ANT-TERM.
           MOVE 0   TO A4-START-DT.
           MOVE " " TO A4-REBATED.
           MOVE 0   TO A4-WORK-AMT.
           MOVE 0   TO A4-ORIG-ANT.
           MOVE 0   TO A4-ADJ-ANT.
           MOVE 0   TO A4-NET-ANT.
           MOVE 0   TO A4-EARNINGS.
           MOVE 0   TO A4-UNEARNED.
           MOVE 0   TO A4-UPFRONT.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      *********************************************************
       WRITE-DETAIL-LINE SECTION.
      *********************************************************
           MOVE DETAIL-LINE  TO  PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO  LN-FEED.
           MOVE " "       TO  DETAIL-LINE.


      *********************************************************
       HEAD-PAGE SECTION.
      *********************************************************
           MOVE "N"         TO  FIRST-TIME.
           ADD 1            TO  PAGE-NUMBER.
           MOVE PAGE-NUMBER TO  H-PAGE-NO.

           PERFORM GET-TIME.
           MOVE T-HH        TO  H-HOUR.
           MOVE T-MM        TO  H-MIN.

           MOVE HEAD1       TO  PR-REC.
           MOVE 99          TO  LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - -WRITTEN-HEAD1-

           MOVE HEAD2       TO  PR-REC.
           MOVE 1           TO  LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - -WRITTEN-HEAD2-

           MOVE ALL "-"     TO  PR-REC.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 1         TO  LN-FEED.
       EXIT-HEAD-PAGE.
           EXIT.

      *********************************************************
       PRINT-RANGE-LINES SECTION.
      *********************************************************
           MOVE 2                        TO  LN-FEED.
           MOVE "BEGINNING LOAN NUMBER"  TO  RANGE-LINE.
           MOVE BEG-LN              TO  RANGE-SELECTION.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING LOAN NUMBER"      TO  RANGE-LINE.
           MOVE END-LN              TO  RANGE-SELECTION.

           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO AOLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AOLIST-SYSDATE.
           DISPLAY AOLIST-SCREEN UPON AOLIST-WINDOW-HANDLE.
           MOVE AOLIST-FORM-FIELDS TO WR-BUF.
           MOVE AOLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE AOLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE AOLIST-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/AOLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       DISPLAY-FIELDS-COMPONENTS SECTION.
      *********************************************************
           IF LN-PLCD = " " AND LN-SERVACCT NOT = "Y"
              PERFORM CALC-ANTIC
              IF A4-ALP NOT = " "
                 PERFORM DISPLAY-FIELDS-COMPONENTS-RB
              ELSE
                 MOVE WS-UNEARNED          TO A4-UNEARNED
           ELSE
              IF LN-PLCD NOT = " "
                 MOVE "NONE (PL)" TO A4-ALP
              ELSE
                 IF LN-SERVACCT = "Y"
                    MOVE "NONE (SA)" TO A4-ALP.

       DISPLAY-FIELDS-COMPONENTS-RB.

      * SKIP REBATED COMPONENTS:

           IF ( LTI-ANTICADJ(1) NOT = 0
                      OR (NOT (LTI-INT-REBATE = " " OR "C")) )
              MOVE "  REBATED?" TO A4-ALP.

       DISP-FIELDS-EXIT.
           EXIT.

      *********************************************************
       BWLN-SCAN SECTION.
      *********************************************************
           MOVE EXT-CONAME-SYSDATE TO BWLNSC-SYSDATE.
           MOVE 0                  TO BWLNSC-ACCTNO.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BWLN-SCAN.

           IF (LOAN-FG = "BEG" )
                MOVE "SNNN060LOANNO1." TO VDU
           ELSE
                MOVE "SNNN060LOANNO2." TO VDU.

           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BWLNSC-ACCTNO TO VDUNUM0.

           MOVE 0 TO IO-FG.

       EXIT-BWLN-SCAN.
           EXIT.

      *********************************************************
       MISC SECTION.
      *********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
      *--------------------------------------------------------
      *-----------------------------------------------------------
       CALC-ANTIC.
           ADD      LTI-ANTICADJ(1)  TO     LTI-ANTICERN(1)
                                    GIVING WS-ORIG-ANTIC.
           SUBTRACT LTI-ACCERN(1)    FROM   LTI-ANTICERN(1)
                                    GIVING WS-UNEARNED.

      *-----------------------------------------------------------
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO CDB-REC.

      *********************************************************
       IO-ROUTINE SECTION.
      *********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************************************
       END-PROGRAM SECTION.
      *********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY AOLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
