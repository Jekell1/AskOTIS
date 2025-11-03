       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPLINE.
       DATE-WRITTEN.    122391.
      **************************************************************************
      *          WINDOW LOAN PAYMENT LINES
      *            CALLED FROM SPINQ/XPINQ, LONPG0,G3,G5,G7 
      *
      *  REV:
      *  101794 BAH REMOVED ALL OCCURANCES OF LP2
      *  053095 SLC COMBINED OTHBAL AND OT2BAL IN L-090
      *  MJD 071296 ADDED LPLINE-UNITPER-CD (LN-UNITPER-CD).
      *             TO TEST FOR WEEKLY/MONTHLY PDTH DATE
      *  BAH 121996 ADDED L2-BUF WITH A LINE DISPLAY THAT CAN HANDLE
      *             APCUR OF > 99999.99 FOR ADVANCED CREDIT, #421
      *  BAH 050997 ALWAYS DISPLAY BALANCE, EVEN ON REVERSALS, #487
      *  MJD 061997 CHENGED SETUP-LINE AND SETUP-LINE2 TO MATCH WHAT IS
      *             DISPLAYED IN LONPF1.C.
      *  JTG 112597 CHANGED DISPLAY OF APOTH L-050 TO 9(5)V99- COLONIAL TN
      *  KEC 031098 ADDED 'F4-TOGGLE ON DATE' OPTION.  TOGGLE ON DATE
      *              FROM LP-PAYDATE TO LP-TRDATE AND BACK.
      *  MJD 083398 ADDED ENVIRONMENT VARIABLE TO DETERMINE WHETHER OR NOT
      *             TO DISPLAY BALANCES ON REVERSALS. FIXED DISPLAY TO INDICATE
      *             INCLUDED O2 BAL WITH "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      *  MJD 001019 CHANGED TO ACCEPT PAGE-UP AND PAGE-DOWN (AS PER JTG)
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      *  BLV 020703 ADDED LOAN FILE TO READ TO GET UNITPER-CODE..., JOHN
      *             EXPANDED LPLINE-WORKER WHEN HE EXPANDED WICL-WORKER IN
      *             1996, THIS KNOCKED MIKES LPLINE-UNITPER-CD OUT OF THE
      *             BUFFER SINCE SCAN-BUF IS ONLY 117 BYTES LONG & LPLINE
      *             WORKER IS NOW 120, SO GO FIGURE, I GUESS WE SHOULD ONLY
      *             USE THE FIRST 96 BYTES OR SO OF LPLINE-WORKER.....
      *             JUDY, UNITED
      * BAH 090204 FIX LARGE APOTH ON "AP" AND ALSO ON PAYMENTS,REGACC PL# 649
      *  CS 130605 A30 COMPLETED
      * BAH 170419 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 190103 GLOBAL LPFILE
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 20220803 HARD CODED START
      *
      * JKC 2022-0923 CHANGED THE HARDCODING OF THE STARTS OF LPFILE.
      *
      * JKC 2022-0930 CHANGED TO USE START-LP1-FILE-SCAN INSTEAD OF
      *          START-LP1-FILE; NEED TO USE START-LP1-FILE-SCAN WHEN
      *          START-LP1-FILE-NOT-GREATER IS USED FIRST; NEEDED FOR
      *          FOR DISPLAYING MULTIPLE PAGES IN AN UP & DOWN FUNCTION;
      *          NEED TO ADD ONE TO LP-SEQNO (IN WBEG) IN ORDER TO GET 
      *          THE DISPLAY TO WORK LIKE WITH VISION FILES.
      *
      * JKC 2023-0921 CHANGES DISPLAY-ENTRY-DOWN SECTION TO MATCH HOW IN
      *          LP/LONPF1 WHICH IS WORKING AS EXPECTED; LORI WAS
      *          GETTING AN READ ERROR ON THE LPFILE (RDNXCR; NO CURSOR)
      *          WHICH STOPS PROCESSING.  IN THIS PROGRAM, THE START
      *          OF DISPLAY-ENTRY-DOWN WAS ONLY DOING A 
      *          START-LP1-FILE-SCAN ONLY IF A READ ON THE LPFILE WAS
      *          NOT FOUND; THUS IT ONLY DOES A START IF NO LP-REC
      *          WAS FOUND BUT WOULD STILL DO A READ-LP1-FILE-NEXT
      *          RESULTING AN IN RDNXCR ERROR BECAUSE NO START WAS DONE.
      * BAH 2024.0418 REMOVED LPLINEW, NOTHING NEEDED PASSED, USE EXTERNALS
      *          
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
           "@(#)WI/LPLINE.CBL S35 04/24/24-14:43:18 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  LPLINE-LINES              PIC 99  VALUE 13.
       01  LPLINE-COLUMNS            PIC 99  VALUE 78.
       01  LPLINE-BEGIN-Y            PIC 99  VALUE 02.
       01  LPLINE-BEGIN-X            PIC 99  VALUE 10.
       01  LPLINE-WINDOW-HANDLE PIC X(10).
       01  ERRCD               PIC X        VALUE " ".
       01  SUB                 PIC 9(2)     VALUE 0.
       01  DISP-REV-BAL        PIC X        VALUE "Y".
       01  HOLD1-KEY-DOWN      PIC X(17)    VALUE SPACES.
       01  HOLD2-KEY-UP        PIC X(17)    VALUE SPACES.
       01  FULL-PAGE           PIC X        VALUE " ".
       01  ENTRY-FLAG          PIC 9        VALUE 0.
       01  LN-WK               PIC 9(3) COMP-3  VALUE 0.
      **************************************************
      *         ORDER LINE BUFFER                      *
      **************************************************
       01  LINE-LIST.
           03  LINE-LIST-TABLE.
               05  FILLER               PIC X(12)  VALUE
               "L01 L01X    ".
               05  FILLER               PIC X(12)  VALUE
               "L02 L02X    ".
               05  FILLER               PIC X(12)  VALUE
               "L03 L03X    ".
               05  FILLER               PIC X(12)  VALUE
               "L04 L04X    ".
               05  FILLER               PIC X(12)  VALUE
               "L05 L05X    ".
               05  FILLER               PIC X(12)  VALUE
               "L06 L06X    ".
               05  FILLER               PIC X(12)  VALUE
               "L07 L07X    ".
               05  FILLER               PIC X(12)  VALUE
               "L08 L08X    ".
           03  FILLER                  REDEFINES LINE-LIST-TABLE.
               05  LINE-LIST-TBL        OCCURS 8 TIMES.
                   07  FILLER           PIC X(8).
                   07  LINE-LIST-ATT    PIC X(3).
                   07  FILLER           PIC X.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LINE-BUFFER         VALUE SPACES.
           03  D-LINE          OCCURS 8
                               PIC X(78).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  TOGGLE-BUFFER                                   VALUE SPACES.
           03  T-LINE                                      OCCURS 8.
               05  T-DATE                     PIC 9(06).
               05  T-DATE-SIGNAL              PIC X(01).
               05  FILLER                     PIC X(73).

       01  TOGGLE-DATES-BUFFER                             VALUE ZEROES.
           03  TOGGLE-DATES-TABLE                          OCCURS 8.
               05  TOGGLE-PAYDATE             PIC 9(06).
               05  TOGGLE-TRANS-DATE          PIC 9(06).

       01  TOGGLE-DATE                        PIC X(01)    VALUE "P".
           88  TOGGLE-DATE-PAYDATE                         VALUE "P".
           88  TOGGLE-DATE-TRANS-DATE                      VALUE "T".

       01  TOGGLE-FIRST-TIME                  PIC X(01)    VALUE "Y".
           88  TOGGLE-FIRST-TIME-NO                        VALUE "N".
           88  TOGGLE-FIRST-TIME-YES                       VALUE "Y".

      *-----------------------------------------------------------------
       01  LN-BUF              VALUE SPACES.
           03  L-010           PIC 9(6).
           03  L-DATE-SIGNAL   PIC X.
           03  L-020           PIC XX.
           03  FILLER          PIC X.
           03  L-030           PIC X(5).
      * GROUP SIZE = 24
           03  L-WK.
               05  L-040           PIC Z(5).99-  BLANK ZERO.
               05  L-040RATE      REDEFINES L-040
                                   PIC ZZZZ.999-.
               05  FILLER         REDEFINES L-040.
                   07  L-040D      PIC X.
                   07  L-040DLNO   PIC 9999.99-.

               05  L-050           PIC Z(3).99-  BLANK ZERO.
               05  L-060           PIC Z(4).99-  BLANK ZERO.
      * USED FOR AP TRANSACTIONS:
           03  FILLER             REDEFINES L-WK.
               05  FILLER          PIC X(7).
               05  L-050-Z5        PIC Z(5).99-  BLANK ZERO.
               05  FILLER          PIC X(8).
      * USED FOR LARGE APOTH TRANSACTIONS
           03  FILLER             REDEFINES L-WK.
               05  FILLER          PIC X(9).
               05  L-050-SH        PIC Z(4).99-  BLANK ZERO.
               05  L-060-SH        PIC Z(3).99-  BLANK ZERO.
           03  L-070           PIC Z(5).99-  BLANK ZERO.
           03 L-080-MO         PIC 99.
           03 L-080-DAYR       PIC 99.
           03  L-090           PIC Z(5).99-  BLANK ZERO.
           03  L-100           PIC Z(4).99-  BLANK ZERO.
           03  L-110           PIC Z(5)9.99.
           03  FILLER         REDEFINES L-110.
               05  FILLER      PIC X(8).
               05  L-120       PIC X.
      * DISPLAY BUFFER USED IS PRINCIPAL IS OVER 99,999.99
      * LEFT OUT PAID THRU, DOESNT MATTER ON A PAYOFF
       01  LN2-BUF            REDEFINES LN-BUF.
           03  L2-010          PIC 9(6).
           03  L2-DATE-SIGNAL  PIC X.
           03  L2-020          PIC XX.
           03  FILLER          PIC X.
           03  L2-030          PIC X(5).
           03  L2-040          PIC Z(7).99- BLANK ZERO.
           03  L2-040RATE     REDEFINES L2-040
                               PIC ZZZZZZ.999-.
           03  FILLER         REDEFINES L2-040.
               05  FILLER      PIC X.
               05  L2-040D     PIC X.
               05  FILLER      PIC X.
               05  L2-040DLNO  PIC 9999.99-.
           03  L2-050          PIC Z(4).99- BLANK ZERO.
           03  L2-060          PIC Z(5).99- BLANK ZERO.
           03  L2-070          PIC Z(6).99- BLANK ZERO.
           03  FILLER          PIC X.
           03  L2-090          PIC Z(5).99- BLANK ZERO.
           03  L2-100          PIC Z(4).99- BLANK ZERO.
           03  L2-110          PIC Z(5)9.99.
           03  L2-120          PIC X.

       01  LPLINE-LEGEND1.
           03  FILLER          PIC X(47) VALUE
           " F4-DATE, F7-EXIT, SCROLL LINES (UP)-^, (DOWN)-".
           03  FILLER          PIC X      VALUE X'76'.
           03  FILLER          PIC X      VALUE ":".

      *-----------------------------------------------------------------
       01  LEGEND                PIC X(70).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LPLINE_DEF.CPY".
       COPY "LIBWI/LPLINE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LPLINE_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LPLINE" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
              AT  SCREEN LINE LPLINE-BEGIN-X * VDU-WIN-L-OFFSET
              AT  SCREEN COL  LPLINE-BEGIN-Y * VDU-WIN-C-OFFSET
              CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
              SIZE    LPLINE-COLUMNS + VDU-WIN-W-MOD
              LINES   LPLINE-LINES + VDU-WIN-H-MOD
              BLANK SCREEN
              HANDLE IS LPLINE-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE " " TO VDU-WIMENU-SELECTION.

      * TEST ENVIRONMENT VARIABLE "RVLINES" TO DETERMINE WHETHER
      * OR NOT TO DISPLAY BALANCES ON REVERSALS.

           MOVE "RVLINES" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE "N" TO DISP-REV-BAL
           ELSE
              MOVE "Y" TO DISP-REV-BAL.

       MAIN-MODULE.
           MOVE SPACES TO HOLD1-KEY-DOWN.

           MOVE EXT-WI-CL-ACCTNO TO LP-ACCTNO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           PERFORM CLOSE-LN1-FILE.
           MOVE "Y" TO FULL-PAGE.

      * LP-FILE ISNT OPEN YET SO LEAVE THIS LN-OWNBR
           MOVE LN-OWNBR TO LP-BRNO.
           MOVE 99999    TO LP-SEQNO.
           MOVE LP1-KEY  TO HOLD2-KEY-UP.
           PERFORM DISPLAY-ENTRY-UP.

           PERFORM DISPLAY-PAYMENT-LINES.

           GO TO END-ROUTINE.

      ******************************************
       DISPLAY-PAYMENT-LINES SECTION.
      ******************************************
       DISPLAY-PAYMENTS.
           MOVE LPLINE-LEGEND1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF ( F4-KEY )
              PERFORM TOGGLE-ON-DATE
              GO TO DISPLAY-PAYMENTS.
           IF UP-KEY OR PAGE-UP-KEY
              PERFORM DISPLAY-ENTRY-UP
              GO TO DISPLAY-PAYMENTS.
           IF DOWN-KEY OR PAGE-DOWN-KEY
              PERFORM DISPLAY-ENTRY-DOWN
              GO TO DISPLAY-PAYMENTS.
           IF VDUSTATUS NOT = "1>"
              GO TO DISPLAY-PAYMENTS.

       DISPLAY-PAYMENT-LINES-EXIT.
           EXIT.

      **************************************************
       DISPLAY-ENTRY-DOWN SECTION.
      **************************************************
           MOVE " " TO LINE-BUFFER LN-BUF.
           MOVE 0 TO LN-WK.
           MOVE "Y" TO TOGGLE-FIRST-TIME.
           MOVE HOLD1-KEY-DOWN TO LP1-KEY.
           IF LP-SEQNO = 99999
              MOVE 9 TO IO-FG
              GO TO DISPLAY-DOWN-EXIT.

           MOVE LP1-KEY TO HOLD2-KEY-UP.
           PERFORM OPEN-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ***NOTE*** 'START-LP1-FILE-SCAN' MUST BE USED WHEN FIRST USING
      * 'START-LP1-FILE-NOT-GREATER'; USED WHEN DISPLAYING MULTIPLE
      * PAGES IN AN UP & DOWN FUNCTION; YOU NEED TO ADD ONE TO 
      * LP-SEQNO (IN WBEG) IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           COMPUTE QLP1-WBEG-SEQNO = LP-SEQNO + 1.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
           IF ( LP-BRNO = 9999 )
              MOVE LP-BRNO    TO QLP1-WSBEG-BRNO
           ELSE
              COMPUTE QLP1-WSBEG-BRNO = LP-BRNO + 1.
   
           IF ( LP-ACCTNO = 99999999 )
              MOVE LP-ACCTNO  TO QLP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QLP1-WSBEG-ACCTNO = LP-ACCTNO + 1.

           PERFORM START-LP1-FILE-SCAN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO ENTRY-FLAG.

       READ-LPFILE1-NEXT.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO BYPASS-LPFILE1-READ.

           PERFORM READ-LP1-FILE-NEXT.
           IF (LP-ACCTNO NOT = EXT-WI-CL-ACCTNO) OR
                 (LP-BRNO NOT = LP-PATH-OWNBR)
              IF IO-FG = 0
                 ADD 1 TO ENTRY-FLAG
                 GO TO READ-LPFILE1-NEXT.
       BYPASS-LPFILE1-READ.
           ADD 1 TO LN-WK.
           IF IO-FG = 0
              PERFORM SETUP-LINE
              MOVE LN-WK TO SUB
              PERFORM SET-TOGGLE-BUFFER.

           MOVE LN-BUF TO D-LINE(LN-WK).
           IF LP-CURBAL < 0
              MOVE ":F"  TO LINE-LIST-ATT(LN-WK)
           ELSE
              MOVE " "   TO LINE-LIST-ATT(LN-WK).

           MOVE SPACES TO LN-BUF.
           IF LN-WK = 8
              GO TO DISPLAY-XFER-DOWN.
           IF IO-FG = 0
              GO TO READ-LPFILE1-NEXT.
           IF FULL-PAGE = "Y"
              MOVE "N" TO FULL-PAGE
              MOVE LN-OWNBR         TO LP-BRNO
              MOVE EXT-WI-CL-ACCTNO TO LP-ACCTNO
              MOVE 99999            TO LP-SEQNO
              MOVE LP1-KEY          TO HOLD2-KEY-UP
              PERFORM DISPLAY-ENTRY-UP
              GO TO DISPLAY-DOWN-EXIT.
       DISPLAY-XFER-DOWN.
           PERFORM DISPLAY-ALL-LINES.

       DISPLAY-DOWN-EXIT.
           IF IO-FG = 0
              MOVE "Y" TO FULL-PAGE
              MOVE LP1-KEY TO HOLD1-KEY-DOWN
           ELSE
              MOVE EXT-WI-CL-OWNBR  TO LP-BRNO
              MOVE EXT-WI-CL-ACCTNO TO LP-ACCTNO
              MOVE 99999            TO LP-SEQNO
              MOVE LP1-KEY          TO HOLD1-KEY-DOWN.
           PERFORM CLOSE-LP1-FILE.

      ********************************************
       DISPLAY-ENTRY-UP SECTION.
      ********************************************
           MOVE " " TO LINE-BUFFER LN-BUF.
           MOVE 9 TO LN-WK.
           MOVE "Y" TO TOGGLE-FIRST-TIME.
           MOVE HOLD2-KEY-UP TO LP1-KEY.
           IF LP-SEQNO = 0
              MOVE 9 TO IO-FG
              GO TO DISPLAY-UP-EXIT.

           MOVE LP1-KEY TO HOLD1-KEY-DOWN.

           PERFORM OPEN-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           MOVE LP-SEQNO      TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.

           MOVE 0 TO ENTRY-FLAG.
       READ-LPFILE2-NEXT.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO BYPASS-LPFILE2-READ.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF (LP-ACCTNO NOT = EXT-WI-CL-ACCTNO) OR
               (LP-BRNO NOT = LP-PATH-OWNBR)
              IF IO-FG = 0
                 ADD 1 TO ENTRY-FLAG
                 GO TO READ-LPFILE2-NEXT.
       BYPASS-LPFILE2-READ.
           SUBTRACT 1 FROM LN-WK.
           IF LN-WK < 1
              MOVE "Y" TO FULL-PAGE
              GO TO DISPLAY-XFER-UP.
           IF IO-FG = 0
              PERFORM SETUP-LINE
              MOVE LN-WK TO SUB
              PERFORM SET-TOGGLE-BUFFER.

           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACES TO LN-BUF.
           IF IO-FG = 0
              GO TO READ-LPFILE2-NEXT.
           IF FULL-PAGE = "Y"
              MOVE "N" TO FULL-PAGE
              MOVE EXT-WI-CL-OWNBR   TO LP-BRNO  
              MOVE EXT-WI-CL-ACCTNO TO LP-ACCTNO
              MOVE 0                TO LP-SEQNO
              MOVE LP1-KEY          TO HOLD1-KEY-DOWN
              PERFORM DISPLAY-ENTRY-DOWN
              GO TO DISPLAY-UP-EXIT.
       DISPLAY-XFER-UP.
           PERFORM DISPLAY-ALL-LINES.

       DISPLAY-UP-EXIT.
           IF IO-FG = 0
              MOVE "Y" TO FULL-PAGE
              MOVE LP1-KEY TO HOLD2-KEY-UP
           ELSE
              MOVE EXT-WI-CL-OWNBR  TO LP-BRNO  
              MOVE EXT-WI-CL-ACCTNO TO LP-ACCTNO
              MOVE 0                TO LP-SEQNO
              MOVE LP1-KEY TO HOLD2-KEY-UP.
           PERFORM CLOSE-LP1-FILE.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE SPACES TO LN-BUF.
           IF LP-APCUR > 99999.99 OR LP-APCUR < -99999.99
              PERFORM SETUP-LINE2
              GO TO END-SETUP-LINE.

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L-010
           ELSE
              MOVE LP-TRDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L-010.

           IF LP-TRDATE NOT = LP-PAYDATE
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE "."          TO L-DATE-SIGNAL
              ELSE
      * LOWER CASE T
                 MOVE X'74' TO L-DATE-SIGNAL.

           MOVE LP-TRCD TO L-020.

           MOVE LP-REFNO TO L-030 LPWSLP-LP-REFNO.
           IF LP-TRCD = "AP"
              MOVE LP-APOTH TO L-050-Z5
           ELSE
              IF LP-TRCD = "JD" OR "AC" OR "IR"
                 MOVE LP-NEWSTAT-RATE TO L-040RATE
              ELSE
                 IF (LP-TRCD = "P4" OR "P5")
                          AND (LP-DLNO NOT= 0)
                    MOVE "D" TO L-040D
                    MOVE LP-DLNO TO L-040DLNO
                 ELSE
                    MOVE LP-TRAMT TO L-040
                    IF ( (LP-TRCD = "RT" OR "ET" OR "LP"
                                          OR "NP" OR "ST")
                             OR (LP-TRCD = "RV"
                                   AND (LPWSLP-LP-ADDON-TEXT = "ADD"
                                                            OR "ADJ"
                                                            OR "CNL")
                                )
                       )
                       IF LP-IBPC = "P"
                          MOVE LP-APCUR TO L-040
                       END-IF
                    END-IF
                 END-IF
              END-IF
              IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                    OR ( (LP-TRCD = "RV") AND
                          (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                               OR "RB" OR "RO")
                       )
                 )  AND (LP-APOT2 NOT = 0)
                 ADD LP-APOTH LP-APOT2 GIVING L-050
                 INSPECT L-050 REPLACING TRAILING " " BY "*"
              ELSE
                 MOVE LP-APOTH TO L-050.

           ADD LP-APINT LP-APLC GIVING L-060.
           MOVE LP-APCUR TO L-070.
           IF LP-TRCD = "AP"
              MOVE LP-APINTOWE TO L-050-Z5
              ADD LP-LCPARTIALS LP-INTDUE GIVING L-060
              COMPUTE L-070 = 0 - LP-APINTOWE
                                - LP-INTDUE
                                - LP-LCPARTIALS
           ELSE
              IF ( (LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST")
                       OR (LP-TRCD = "RV"
                             AND (LPWSLP-LP-ADDON-TEXT = "ADD"
                                                      OR "ADJ"
                                                      OR "CNL")
                          )
                 )
                 IF LP-IBPC = "P"
                    MOVE LP-TRAMT TO L-070.

      * REGACC PL# 649  LARGE APOTH GETTING TRUNCATED
           IF LP-TRCD NOT = "AP"
              IF (LP-APOTH > 999.99 OR < -999.99)
                 IF  (LP-APINT + LP-APLC) < 1000.00
                    MOVE LP-APOTH TO L-050-SH
                    ADD LP-APLC LP-APINT GIVING L-060-SH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 5/9/97 BAH ALWAYS DISPLAY BALANCE, #487
      * 8/11/98 SUPPRESS BALANCE IF ENV 'RVLINES' IS SET (MJD AS PER JTG)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0) AND
                   DISP-REV-BAL = "N"
              MOVE "*" TO L-120
           ELSE
              MOVE LP-PDTH-DATE TO NUM-DATE
              MOVE NUM-MO       TO L-080-MO
              IF LN-UNITPER-CD = "D" OR "S" OR "W" OR "B"
                 MOVE NUM-DA TO L-080-DAYR
              ELSE
                 MOVE NUM-YR TO L-080-DAYR
              END-IF
              COMPUTE L-090 = LP-OTHBAL + LP-OT2BAL
              ADD LP-INTBAL LP-LCBAL GIVING L-100
              MOVE LP-CURBAL TO L-110.
       END-SETUP-LINE.
           EXIT.

      ***********************************************
       SETUP-LINE2 SECTION.
      ***********************************************

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L2-010
           ELSE
              MOVE LP-TRDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO L2-010.

           IF LP-TRDATE NOT = LP-PAYDATE
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE "."          TO L2-DATE-SIGNAL
              ELSE
      * LOWER CASE T
                 MOVE X'74' TO L2-DATE-SIGNAL.

           MOVE LP-TRCD TO L2-020.

           MOVE LP-REFNO TO L2-030 LPWSLP-LP-REFNO.

           IF LP-TRCD = "JD" OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO L2-040RATE
           ELSE
              IF (LP-TRCD = "P4" OR "P5")
                       AND (LP-DLNO NOT = 0)
                 MOVE "D" TO L2-040D
                 MOVE LP-DLNO TO L2-040DLNO
              ELSE
                 MOVE LP-TRAMT TO L2-040
                 IF ( (LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST")
                          OR (LP-TRCD = "RV"
                                AND (LPWSLP-LP-ADDON-TEXT = "ADD"
                                                         OR "ADJ"
                                                         OR "CNL")
                             )
                    )
                    IF LP-IBPC = "P"
                       MOVE LP-APCUR TO L2-040.

           IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 OR ( (LP-TRCD = "RV") AND
                       (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                            OR "RB" OR "RO")
                    )
              )  AND (LP-APOT2 NOT = 0)
              ADD LP-APOTH LP-APOT2 GIVING L2-050
              INSPECT L2-050 REPLACING TRAILING " " BY "*"
           ELSE
              MOVE LP-APOTH TO L2-050.
           ADD LP-APINT LP-APLC GIVING L2-060.
           MOVE LP-APCUR TO L2-070.
           IF LP-TRCD = "AP"
              MOVE LP-APINTOWE TO L2-050
              ADD LP-LCPARTIALS LP-INTDUE GIVING L2-060
              COMPUTE L2-070 = 0 - LP-APINTOWE
                                 - LP-INTDUE
                                 - LP-LCPARTIALS
           ELSE
              IF ( (LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST")
                       OR (LP-TRCD = "RV"
                             AND (LPWSLP-LP-ADDON-TEXT = "ADD"
                                                      OR "ADJ"
                                                      OR "CNL")
                          )
                 )
                 IF LP-IBPC = "P"
                    MOVE LP-TRAMT TO L2-070.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 6/19/97 MJD ALWAYS DISPLAY BALANCE
      * 8/11/98 SUPPRESS BALANCE IF ENV 'RVLINES' IS SET (MJD AS PER JTG)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0) AND
                   DISP-REV-BAL = "N"
              MOVE "*" TO L2-120
           ELSE
              COMPUTE L2-090 = LP-OTHBAL + LP-OT2BAL
              ADD LP-INTBAL LP-LCBAL GIVING L2-100
              MOVE LP-CURBAL TO L2-110
              IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
                             NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
                 MOVE "*" TO L2-120.

      ******************************************************************
       SET-TOGGLE-BUFFER SECTION.
      ******************************************************************

           IF ( TOGGLE-FIRST-TIME-YES )
              MOVE "N"    TO TOGGLE-FIRST-TIME
              MOVE SPACES TO TOGGLE-BUFFER
              MOVE SPACES TO TOGGLE-DATES-BUFFER.

           MOVE LP-PAYDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TOGGLE-PAYDATE(SUB).
           MOVE LP-TRDATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TOGGLE-TRANS-DATE(SUB).

           MOVE LN-BUF      TO T-LINE           (SUB).

       EXIT-SET-TOGGLE-BUFFER.
           EXIT.

      ******************************************************************
       TOGGLE-ON-DATE SECTION.
      ******************************************************************

           IF ( TOGGLE-DATE-PAYDATE )
              MOVE "T" TO TOGGLE-DATE
           ELSE
              MOVE "P" TO TOGGLE-DATE.

           MOVE SPACES TO LINE-BUFFER.
           MOVE ZEROES TO SUB.

       NEXT-TOGGLE-ON-DATE.

           ADD 1 TO SUB.

           IF ( SUB > 8 )
              GO TO END-TOGGLE-ON-DATE.

           IF ( TOGGLE-PAYDATE   (SUB) NOT NUMERIC ) AND
              ( TOGGLE-TRANS-DATE(SUB) NOT NUMERIC )
              GO TO NEXT-TOGGLE-ON-DATE.


           IF TOGGLE-TRANS-DATE(SUB) NOT = TOGGLE-PAYDATE(SUB)
              IF ( TOGGLE-DATE-PAYDATE )
                 MOVE TOGGLE-PAYDATE   (SUB) TO T-DATE       (SUB)
                 MOVE "."                    TO T-DATE-SIGNAL(SUB)
              ELSE
                 MOVE TOGGLE-TRANS-DATE(SUB) TO T-DATE       (SUB)
      * LOWER CASE T
                 MOVE X'74'           TO T-DATE-SIGNAL(SUB)
           ELSE
              MOVE TOGGLE-PAYDATE      (SUB) TO T-DATE       (SUB)
              MOVE SPACES                    TO T-DATE-SIGNAL(SUB).

           MOVE T-LINE(SUB) TO D-LINE(SUB).

           GO TO NEXT-TOGGLE-ON-DATE.

       END-TOGGLE-ON-DATE.

           PERFORM DISPLAY-ALL-LINES.

       EXIT-TOGGLE-ON-DATE.
           EXIT.


      ************************************
       FORM-RESET SECTION.
      ************************************
           DISPLAY LPLINE-SCREEN UPON LPLINE-WINDOW-HANDLE.
           MOVE "S  A024TITLE." TO VDU.
           MOVE " PAYMENT HISTORY "      TO VDUBUF.
           PERFORM SCREENIO.
           MOVE LPLINE-HEAD TO WR-BUF.
           MOVE "HEAD." TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE LPLINE-FORM-FIELDS TO WR-BUF.
           MOVE LPLINE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE LPLINE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPLINE-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LPLINE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-ALL-LINES.
           MOVE LINE-LIST TO WR-LIST.
           MOVE LINE-BUFFER TO WR-BUF.
           PERFORM CALL-WRFORM.


      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPLINE-SCREEN.
           DESTROY LPLINE-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
