       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BWTXT.
       DATE-WRITTEN. 062314
      *********************************************************************
      *                      (LP)
      *    DESCRIPTION:  BORROWER TEXT OPT-IN REGISTER
      *               
      *        WORLD PR# 1000
      *        REPORT WILL READ BORROWER FILE, INCLUDING BORROWERS
      *        WITH SELECTED TEXT OPT IN CODE.  INCLUDES OPEN, NON-PL
      *        LOANS, & OPTIONALLY, SKIP, INCLUDE ONLY ON PAID-OUTS.
      * REV:
      * BLM 141003 ADD EXTRA BUCKET FOR CODES TO INCLUDE FOR TYPE "D"
      *            (DECLINED) & ADD OPTION FOR F3-ALL CODES, ADD EXTRA
      *            BUCKETS TO TOTALS FOR "D" & "OTHER", WORLD PR# 1024
      *  CS 151215 ADDED LOAN DATE RANGE  PR#1126 WORLD
      *   *NOTE* THIS IS NOT THE TYPICAL SKIP,INC,ONLY OPTION.
      *          BORROWERS THAT HAVE OPEN LOANS(NON PL) AND IF SELECTED
      *          BORROWERS THAT HAVE ONLY A PAID OUT ACCOUNT WITHIN THE DATE
      *          RANGE 
      * BAH 170511 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181002 GLOBAL BWFILE
      * BAH 181204 GLOBAL LNFILE
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE, BWFILE
      * RMZ 20230828 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *          BECAUSE OF HOW DATA IS BEING READ ON A15 -VS- S35, THE 
      *          ACCOUNT NUMBER ON THE REPORTS WILL NOT ALWAYS MATCH, 
      *          REGARDLESS IF THE SQL IN THE STORED PROCEDURE SORTS 
      *          THE ACCOUNT NUMBER IN ASCENDING OR DESCENDING ORDER.
      *          READING BY LN4 WHICH IS SSNO ORDER AND THE ACCOUNTS UNDER
      *          IT ARE IN ORDER OF BEING WRITTEN AND THE UPGRADE IMPACTED
      *          THAT ORDER.
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      * 
      *********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *  SUMMARY TOTALS WORK RECORD
      *  OPTIN-TABLE, 1=M, 2=A, 3=B, 4=S, 5=N, 6=D, 7=OTHER
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO     PIC 9(4).
           03  WK-OPTIN-TABLE  OCCURS 7.
               05  WK-NOLOANS  PIC S9(9) COMP.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)LP/BWTXT.CBL S35 12/22/23-11:20:38 >".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
           
       01  PR-IFN               PIC X(2)  VALUE "PR".
       01  WK-IFN               PIC X(2)  VALUE "WK".
 
       01  BW-CONN-OPEN-SW      PIC X     VALUE "N".
 
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 1.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  ER-FEED              PIC 9        VALUE 1.

       01  HOLD-ACCTNO          PIC 9(8)     VALUE 0.
 
       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
       01  TOTAL-TEST           PIC 9(7) VALUE 0.

       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL     OCCURS 10 TIMES.
               05  WS-GTT-OPTIN-TAB OCCURS 7 TIMES.
                   07  GTT-NOLOANS  PIC S9(9) COMP.

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
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(26)    VALUE
               "TEXT OPT-IN AUDIT REGISTER".
 
       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH :  ".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X(2)     VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD4.
           03  FILLER      PIC X(30)    VALUE "NAME         ".
           03  FILLER      PIC X(25)    VALUE 
               "LAST 4 SS     CELL PHONE ".
           03  FILLER      PIC X(20)    VALUE 
               " TEXT OPTIN   ACCT #".
       01  TOTAL-HEAD.
           03  FILLER      PIC X(19)    VALUE 
               "**  TOTALS **".
           03  FILLER      PIC X(28)    VALUE 
               "             M             A".
           03  FILLER      PIC X(28)    VALUE 
               "             B             S".
           03  FILLER      PIC X(28)    VALUE 
               "             N             D".
           03  FILLER      PIC X(15)    VALUE 
               "         OTHER ".
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
 
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-NAME      PIC X(33).
           03  FILLER      PIC X(2).
           03  D-SSNO      PIC 9999.
           03  FILLER      PIC X(4).
           03  D-CELL      PIC 999/999/9999.
           03  FILLER      PIC X(6).
           03  D-OPT-IN    PIC X.
           03  FILLER      PIC X(6).
           03  D-ACCTNO    PIC ZZ9999.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(7).

           03  G-OPTINS     OCCURS 7.
               05  FILLER       PIC X.
               05  G-LOANS      PIC ZZZZ,ZZZ,ZZ9-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-DESC       PIC X(30).
           03  RANGE-SELECTION  PIC X(102).
           03  FILLER           REDEFINES RANGE-SELECTION.
               05  RANGE-ALP1   PIC X(15).
               05  RANGE-NUM1   REDEFINES RANGE-ALP1
                                PIC Z(14)9.
               05  RANGE-DATE1  REDEFINES RANGE-ALP1
                                PIC 9999/99/99.
               05  FILLER       PIC XX.
               05  RANGE-ALP2   PIC X(15).
               05  RANGE-NUM2   REDEFINES RANGE-ALP2
                                PIC Z(14)9.
               05  RANGE-DATE2  REDEFINES RANGE-ALP2
                                PIC 9999/99/99.
               05  FILLER       PIC X(70).
           03  RANGE-TABLE      REDEFINES RANGE-SELECTION.
               05  RANGE-TAB    OCCURS 10 
                                PIC XXXXX.
               05  FILLER       PIC X(52). 

      **********************
      *   MISC WORKERS     *
      **********************
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

       01  CURRENT-COUNT        PIC 9(7)     VALUE 0.
       01  REC-COUNT            PIC 9(7)     VALUE 0.
       01  TOTAL-COUNT          PIC 9(7)     VALUE 0.
       01  TOTAL-FG             PIC X        VALUE "N".
       01  PAIDOUT-COUNT        PIC 9(7)     VALUE 0.
       01  OPEN-COUNT           PIC 9(7)     VALUE 0.
       01  PL-COUNT             PIC 9(7)     VALUE 0.

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  EXITPROG             PIC X(9)     VALUE "LP/LPR2MU".

       01  SUB                  PIC 999.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  FIRST-BW-SW          PIC X        VALUE "Y".
       01  EOF-BW-SW            PIC X        VALUE "Y".
       01  CURR-BW-REC          PIC X(BW-SIZE).
       01  PREV-BW-REC          PIC X(BW-SIZE).
       01  CURR-LN-REC          PIC X(LN-SIZE).
       01  PREV-LN-REC          PIC X(LN-SIZE). 
       01  PREV-BW-SSNO         PIC 9(9)     VALUE 0.


       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-LNDATE       PIC 9(8).
           03  END-LNDATE       PIC 9(8).
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  INCL-OPTIN-BUF.
               05  INCL-OPTIN1  PIC X.
               05  INCL-OPTIN2  PIC X.
               05  INCL-OPTIN3  PIC X.
               05  INCL-OPTIN4  PIC X.
               05  INCL-OPTIN5  PIC X.
               05  INCL-OPTIN6  PIC X.
           03  INCL-PAIDOUTS    PIC X.
           03  BEG-PODATE       PIC 9(8).
           03  END-PODATE       PIC 9(8).
           03  GTT-ISO          PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X OCCURS 10.

       01  VERSION-CONTROL      PIC X VALUE "B".
 
       01  SV-BEG-BR            PIC 9999  VALUE 9999.
       01  SV-END-BR            PIC 9999  VALUE 9999.

       01  INCL-OPTIN-LIST.
           03  FILLER          PIC X(42) VALUE
           "OPTIN1 OPTIN2 OPTIN3 OPTIN4 OPTIN5 OPTIN6.".


       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWTXT_DEF.CPY".
       COPY "LIBLP/BWTXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BWTXT_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BW1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWTXT" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
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
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE "Y" TO NEW-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           IF GTT-ISO NOT = "O"
              MOVE BR-NO TO H-BRNO
              MOVE BR-NAME TO H-BRDESC
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN4-FILE.

           IF EXT-ROUTE-BUF = "00"
              STRING "PROGRAM IN PROGRESS......BRNO: " BR-NO
                       DELIMITED BY SIZE INTO MESS
              MOVE MESS TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.
           MOVE "Y"           TO FIRST-BW-SW.
           MOVE "N"           TO EOF-BW-SW.
           MOVE 0 TO PAIDOUT-COUNT OPEN-COUNT PL-COUNT.

           PERFORM START-BW1-FILE. *> SSP - BW,LN

      ***************************************************************  
      *                                                             *  
      *     A STORED PROCEDURE IS BEING USED TO READ BOTH 'BWFILE & *
      * LNFILE' AT THE SAME TIME. BECAUSE THE PROGRAM BREAKS WHEN   *
      * THE BWFILE CHANGES BRANCH/SSNO, CURRENT AND PREVIOUS LOGIC  *
      * IS NEEDED TO PROCESS THE PERVIOUS SSNO WHENEVER A BREAK     *
      * OCCURS.                                                     *
      *     PREVIOUS = THE SSNO THAT WAS ALREADY PROCESSED IN THE   * 
      *       PROGRAM, BUT NEEDS TO BE AVAILABLE TO DETERMINE IF IT *
      *       NEEDS TO BE ON THE REPORT.                            *
      *                                                             *
      *     CURRENT = THE CURRENT ROW COMING FROM THE STORED        *
      *       TO BE PLACED BACK INTO BW-REC AND LN-REC, ONCE THE    *
      *       PERVIOUS SSNO INFORMATION HAS BEEN PROCESSED.         *
      *                                                             *
      ***************************************************************

       NEXT-BW.
           MOVE BW-REC  TO PREV-BW-REC
           MOVE LN-REC  TO PREV-LN-REC
           MOVE BW-SSNO TO PREV-BW-SSNO 
           PERFORM READ-BW1-FILE-NEXT *> EMBEDDED - BW,LN
           MOVE BW-REC  TO CURR-BW-REC
           MOVE LN-REC  TO CURR-LN-REC.

           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
      *       GO TO END-OF-GROUP.
              MOVE "Y" TO EOF-BW-SW
              GO TO CHECK-LOAN.

           IF FIRST-BW-SW = "Y"
              MOVE "N" TO FIRST-BW-SW
              GO TO CONTINUE-BW.

           IF (BW-SSNO NOT = PREV-BW-SSNO)
              GO TO CHECK-LOAN.
      
       CONTINUE-BW.  

           IF EOF-BW-SW = "Y"
              GO TO END-OF-GROUP.

           IF (BW-SSNO NOT = PREV-BW-SSNO)
              MOVE 0 TO PAIDOUT-COUNT OPEN-COUNT PL-COUNT.

           IF BW-LOANS = 0
              IF INCL-PAIDOUTS = "S"
                 GO TO NEXT-BW. 

      * CHECK BORROWER TEXT OPT IN

           IF INCL-OPTIN-BUF = "******"
              GO TO CONTINUE-PROCESSING.

           IF INCL-OPTIN1 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = INCL-OPTIN1
                 GO TO CONTINUE-PROCESSING.
           IF INCL-OPTIN2 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = INCL-OPTIN2
                 GO TO CONTINUE-PROCESSING.
           IF INCL-OPTIN3 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = INCL-OPTIN3
                 GO TO CONTINUE-PROCESSING.
           IF INCL-OPTIN4 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = INCL-OPTIN4
                 GO TO CONTINUE-PROCESSING.
           IF INCL-OPTIN5 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = INCL-OPTIN5
                 GO TO CONTINUE-PROCESSING.
           IF INCL-OPTIN6 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = INCL-OPTIN6
                 GO TO CONTINUE-PROCESSING.

           GO TO NEXT-BW.

       CONTINUE-PROCESSING.
      *    MOVE 0 TO PAIDOUT-COUNT OPEN-COUNT PL-COUNT.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(1) 
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.

      *    PERFORM START-LN4-FILE.
       NEXT-LNFILE4.
      *    PERFORM READ-LN4-FILE-NEXT.


      *    IF IO-BAD OR (LN-SSNO(1) NOT = BW-SSNO)
      *              OR (LN-OWNBR   NOT = LN-PATH-OWNBR)
      *       GO TO CHECK-LOAN.

           IF LN-POCD-NONLOAN
      *      GO TO NEXT-LNFILE4. 
             GO TO NEXT-BW.     

           IF LN-PLDATE NOT = 0
              ADD 1 TO PL-COUNT
      *       GO TO NEXT-LNFILE4.
              GO TO NEXT-BW.     

           IF LN-POFFDATE NOT = 0
              IF INCL-PAIDOUTS = "S"
      *          GO TO NEXT-LNFILE4 
                 GO TO NEXT-BW     
              ELSE
                 PERFORM CHECK-PAIDOUT-DATE
      *          GO TO NEXT-LNFILE4.
                 GO TO NEXT-BW.     

           IF LN-CURBAL NOT = 0
              PERFORM CHECK-LOANDATE.

      *    GO TO NEXT-LNFILE4.       
           GO TO NEXT-BW.     

       CHECK-LOAN.
           IF INCL-PAIDOUTS = "O"
              IF (PAIDOUT-COUNT > 0 AND OPEN-COUNT = 0 AND PL-COUNT = 0)
                 GO TO PROCESS-BORROWER
              ELSE
      *          GO TO NEXT-BW.
                 GO TO CONTINUE-BW.

           IF OPEN-COUNT = 0
              IF PAIDOUT-COUNT = 0
      *          GO TO NEXT-BW.
                 GO TO CONTINUE-BW.

       PROCESS-BORROWER.

           MOVE PREV-BW-REC TO BW-REC.
           MOVE PREV-LN-REC TO LN-REC.           

           IF GTT-ISO NOT = "O"
              PERFORM CREATE-DETAIL-LINE.

           ADD 1 TO REC-COUNT.
           PERFORM CREATE-WK.
           MOVE "N" TO FIRST-TIME.

           MOVE CURR-BW-REC TO BW-REC.
           MOVE CURR-LN-REC TO LN-REC.
      *    GO TO NEXT-BW.
           GO TO CONTINUE-BW.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN4-FILE.
           IF GTT-ISO NOT = "O"
              PERFORM PRINT-BRANCH-TOTALS.

           ADD REC-COUNT TO TOTAL-COUNT.
           MOVE 0 TO REC-COUNT.
         
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           IF ( GTT-ISO NOT = "S" )
              MOVE 99 TO LN-COUNT
              MOVE "Y" TO TOTAL-FG
              PERFORM GROUP-TOTAL-TABLE.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       CHECK-PAIDOUT-DATE SECTION.
      ******************************************************************
           IF LN-POFFDATE < BEG-PODATE OR LN-POFFDATE > END-PODATE
              GO TO EXIT-CHECK-PAIDOUT.

           ADD 1 TO PAIDOUT-COUNT.
           MOVE LN-ACCTNO TO HOLD-ACCTNO.

       EXIT-CHECK-PAIDOUT.
           EXIT.

      ******************************************************************
       CHECK-LOANDATE SECTION.
      ******************************************************************
           IF LN-LOANDATE < BEG-LNDATE OR LN-LOANDATE > END-LNDATE
              GO TO EXIT-CHECK-LOANDATE.

           ADD 1 TO   OPEN-COUNT.
           MOVE LN-ACCTNO TO HOLD-ACCTNO.

       EXIT-CHECK-LOANDATE.    
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE BWTXT-QUEUE-POS TO Q-POS.
           MOVE BWTXT-COPIES-POS TO C-POS.
           MOVE BWTXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.
           MOVE 0 TO REC-COUNT 
                     CURRENT-COUNT.
           MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.

           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           IF GTT-ISO = "O"
              MOVE "Y" TO TOTAL-FG.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-LNDATE BEG-PODATE.
           MOVE EXT-EOBUF-DATE2 TO END-LNDATE END-PODATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
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
              GO TO REC-OPTIN1.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
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

           GO TO ENTER-LOANDATE1. 

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       ENTER-LOANDATE1.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO REC-OPTIN1.
           MOVE "ENTER BEGINNING LOAN DATE, F3-ALL:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060LNDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "16" GO TO ALL-LNDT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-LOANDATE1.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LNDATE.
           IF BEG-LNDATE = 0 GO TO ENTER-LOANDATE1.

       ENTER-LOANDATE2.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO REC-OPTIN1.
           MOVE "ENTER ENDING LOAN DATE, F3-ALL:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060LNDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-LOANDATE1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-LOANDATE2.
           MOVE VDU-DATE-YYYYMMDD TO END-LNDATE.                  

           IF END-LNDATE < BEG-LNDATE
               MOVE "INVALID DATE RANGE" TO MESS 
               PERFORM SEND-MESS
               GO TO ENTER-LOANDATE1.

           GO TO REC-OPTIN1.

      *-----------------------------------------------------------------
       ALL-LNDT.
           MOVE "S  8080LNDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LNDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080LNDATE2." TO VDU.
           MOVE EXT-JULIAN-END TO END-LNDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       REC-OPTIN1.
           MOVE "ENTER TEXT OPTIN CODE TO INCLUDE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-LOANDATE2.
           IF VDUSTATUS = "16"
              MOVE "******" TO INCL-OPTIN-BUF
              MOVE INCL-OPTIN-BUF TO WR-BUF
              MOVE INCL-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO INCL-PDOUTS-YN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-OPTIN1.
           MOVE VDUBUF TO INCL-OPTIN1.
           IF INCL-OPTIN1 = SPACES
              MOVE SPACES TO INCL-OPTIN-BUF WR-BUF
              MOVE INCL-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO INCL-PDOUTS-YN.
       REC-OPTIN2.
           MOVE "ENTER TEXT OPTIN CODE TO INCLUDE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-OPTIN1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-OPTIN2.
           MOVE VDUBUF TO INCL-OPTIN2.
           IF INCL-OPTIN2 = SPACES
              MOVE SPACES TO INCL-OPTIN3 INCL-OPTIN4
                             INCL-OPTIN5 INCL-OPTIN6
              MOVE INCL-OPTIN-BUF TO WR-BUF
              MOVE INCL-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO INCL-PDOUTS-YN.
       REC-OPTIN3.
           MOVE "ENTER TEXT OPTIN CODE TO INCLUDE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-OPTIN2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-OPTIN3.
           MOVE VDUBUF TO INCL-OPTIN3.
           IF INCL-OPTIN3 = SPACES
              MOVE SPACES TO INCL-OPTIN4 INCL-OPTIN5 INCL-OPTIN6
              MOVE INCL-OPTIN-BUF TO WR-BUF
              MOVE INCL-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO INCL-PDOUTS-YN.
       REC-OPTIN4.
           MOVE "ENTER TEXT OPTIN CODE TO INCLUDE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-OPTIN3.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-OPTIN4.
           MOVE VDUBUF TO INCL-OPTIN4.
           IF INCL-OPTIN4 = SPACES
              MOVE SPACES TO INCL-OPTIN5 INCL-OPTIN6
              MOVE INCL-OPTIN-BUF TO WR-BUF
              MOVE INCL-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO INCL-PDOUTS-YN.
       REC-OPTIN5.
           MOVE "ENTER TEXT OPTIN CODE TO INCLUDE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN5." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-OPTIN4.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-OPTIN5.
           MOVE VDUBUF TO INCL-OPTIN5.
           IF INCL-OPTIN5 = SPACES
              MOVE SPACES TO INCL-OPTIN6
              MOVE INCL-OPTIN-BUF TO WR-BUF
              MOVE INCL-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO INCL-PDOUTS-YN.

       REC-OPTIN6.
           MOVE "ENTER TEXT OPTIN CODE TO INCLUDE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN6." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-OPTIN5.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-OPTIN6.
           MOVE VDUBUF TO INCL-OPTIN6.

      *----------------------------------------------------------------
       INCL-PDOUTS-YN.
           MOVE "E  A010PDOUTS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-OPTIN6.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INCL-PDOUTS-YN.

           MOVE VDUBUF TO INCL-PAIDOUTS.

           IF NOT (INCL-PAIDOUTS = "S" OR "I" OR "O")
              GO TO INCL-PDOUTS-YN.

           IF INCL-PAIDOUTS = "S"
              GO TO GTT-ISO-PARAGRAPH.


       ENTER-PODATE1.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO GTT-ISO-PARAGRAPH.
           MOVE "ENTER BEGINNING PAID OUT DATE, F3-ALL:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060PODATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO INCL-PDOUTS-YN.
           IF VDUSTATUS = "16" GO TO PD-ALL-PODT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-PODATE1.
           MOVE VDU-DATE-YYYYMMDD TO BEG-PODATE.
           IF BEG-PODATE = 0 GO TO ENTER-PODATE1.

       ENTER-PODATE2.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO GTT-ISO-PARAGRAPH.
           MOVE "ENTER ENDING PAID OUT DATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060PODATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-PODATE1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-PODATE2.
           MOVE VDU-DATE-YYYYMMDD TO END-PODATE.

           IF END-PODATE < BEG-PODATE
              GO TO ENTER-PODATE1.

           GO TO PD-END-PODATE.
  
       PD-ALL-PODT.
           MOVE "S  8080PODATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-PODATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080PODATE2." TO VDU.
           MOVE EXT-JULIAN-END TO END-PODATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       PD-END-PODATE.

      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO EXIT-PARM.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )                    
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO BR-01
              ELSE
                 GO TO ENTER-PODATE2.
       
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
           GO TO EXIT-PARM.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTIN1." TO VDU.
           MOVE INCL-OPTIN1 TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTIN2." TO VDU.
           MOVE INCL-OPTIN2 TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTIN3." TO VDU.
           MOVE INCL-OPTIN3 TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTIN4." TO VDU.
           MOVE INCL-OPTIN4 TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTIN5." TO VDU.
           MOVE INCL-OPTIN5 TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPTIN6." TO VDU.
           MOVE INCL-OPTIN6 TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PDOUTS." TO VDU.
           MOVE INCL-PAIDOUTS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO TO VDUBUF.
           PERFORM SCREENIO.

      *******************************************************
       CREATE-DETAIL-LINE SECTION.
      *******************************************************
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                INTO D-NAME.
           MOVE BW-SSNO TO D-SSNO.
           IF BW-CELL-PHONE NOT = 0
              MOVE BW-CELL-PHONE TO D-CELL
              INSPECT D-CELL REPLACING ALL "/" BY "-".
           MOVE BW-PRIVACY-OPT-OUT TO D-OPT-IN.
           MOVE HOLD-ACCTNO TO D-ACCTNO.
           PERFORM WRITE-DETAIL-LINE.

      *******************************************************
       CREATE-WK SECTION.
      *******************************************************
           MOVE BR-NO         TO SAVE-WK-BR.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE SPACES TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              MOVE 0 TO WK-NOLOANS(SUB)
           END-PERFORM.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
      *  OPTIN-TABLE, 1=M, 2=A, 3=B, 4=S, 5=N, 6=D, 7=OTHER
           IF BW-PRIVACY-OPT-OUT = "M"
              ADD 1 TO WK-NOLOANS(1)
           ELSE IF BW-PRIVACY-OPT-OUT = "A"
              ADD 1 TO WK-NOLOANS(2)
           ELSE IF BW-PRIVACY-OPT-OUT = "B"
              ADD 1 TO WK-NOLOANS(3)
           ELSE IF BW-PRIVACY-OPT-OUT = "S"
              ADD 1 TO WK-NOLOANS(4)
           ELSE IF BW-PRIVACY-OPT-OUT = "N"
              ADD 1 TO WK-NOLOANS(5)
           ELSE IF BW-PRIVACY-OPT-OUT = "D"
              ADD 1 TO WK-NOLOANS(6)
           ELSE 
              ADD 1 TO WK-NOLOANS(7).

       UPDATE-WK.
           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
       GTT-ADD-TOTS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              ADD WK-NOLOANS(SUB) TO GTT-NOLOANS(GTT-TOTAL-SUB SUB)
           END-PERFORM.

      ***********************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ***********************************
           MOVE SPACES      TO WK-KEY.
           MOVE WGR-SRESULT TO WK-BRNO.
           PERFORM START-WK-FILE.

        GTT-NEXT-WK1.
           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG   NOT = 0           ) OR
              ( WK-BRNO NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ************************************
           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      *********************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      *********************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      *****************************
       GTT-PRINT-TOTALS SECTION.
      *****************************
       GTT-PRT-TOTALS-NEXT.
           MOVE 0 TO TOTAL-TEST.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              ADD GTT-NOLOANS(GTT-TOTAL-SUB SUB) TO TOTAL-TEST
           END-PERFORM.
           IF TOTAL-TEST = 0
              GO TO EXIT-GTT-PRINT-TOTALS.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              MOVE GTT-NOLOANS(GTT-TOTAL-SUB SUB) TO
                 G-LOANS(SUB)
           END-PERFORM.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      *************************************************
       WRITE-DETAIL-LINE SECTION.
      *************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
 
      *****************************
       PRINT-BRANCH-TOTALS SECTION.
      *****************************
           MOVE 0 TO TOTAL-TEST.
           MOVE BR-NO TO WK-BRNO.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO EXIT-PRINT-BRANCH-TOTALS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              ADD WK-NOLOANS(SUB) TO TOTAL-TEST
           END-PERFORM.
           IF TOTAL-TEST = 0
              GO TO EXIT-PRINT-BRANCH-TOTALS.
        
           MOVE TOTAL-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE "BRANCH:" TO G-PROMPT1.
           MOVE BR-NO     TO G-BRNO WK-BRNO.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              MOVE WK-NOLOANS(SUB) TO G-LOANS(SUB)
           END-PERFORM.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       EXIT-PRINT-BRANCH-TOTALS.
           EXIT.

      *************************************************
       HEAD-PAGE SECTION.
      *************************************************
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
 
           IF TOTAL-FG = "Y"
              MOVE TOTAL-HEAD TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE HEAD3 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD4 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.
 
       END-HEAD-PAGE.
           IF TOTAL-FG = "Y"
              MOVE 3 TO LN-COUNT
           ELSE
              MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *************************************************
       PRINT-RANGE-LINES SECTION.
      *************************************************
           MOVE 4                      TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "SELECTED GROUP             :" TO RANGE-DESC
              MOVE ENT-GROUP                   TO RANGE-ALP1
           ELSE
              MOVE "BRANCH NUMBER RANGE        :" TO RANGE-DESC
              MOVE BEG-BRANCH                  TO RANGE-NUM1
              MOVE END-BRANCH                  TO RANGE-NUM2.
           PERFORM WRITE-DETAIL-LINE.

      *-------------------------------------------------------------
           MOVE "BEG LOAN DATE              :" TO RANGE-LINE
           MOVE BEG-LNDATE                     TO RANGE-DATE1.
           MOVE END-LNDATE                     TO RANGE-DATE2.
           PERFORM WRITE-DETAIL-LINE.

      *--------------------------------------------------------------
           MOVE "TEXT OPTIN CODES TO INCLUDE:" TO RANGE-DESC.
           IF INCL-OPTIN-BUF = "******"
              MOVE "ALL CODES"                 TO RANGE-ALP1
           ELSE
              MOVE INCL-OPTIN1                 TO RANGE-TAB(1)
              MOVE INCL-OPTIN2                 TO RANGE-TAB(2)
              MOVE INCL-OPTIN3                 TO RANGE-TAB(3)
              MOVE INCL-OPTIN4                 TO RANGE-TAB(4)
              MOVE INCL-OPTIN5                 TO RANGE-TAB(5)
              MOVE INCL-OPTIN6                 TO RANGE-TAB(6).
           PERFORM WRITE-DETAIL-LINE.
      *-------------------------------------------------------------
           MOVE "INCLUDE PAIDOUTS           :" TO RANGE-LINE.
           MOVE INCL-PAIDOUTS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *--------------------------------------------------------------
           MOVE "PAYOUT DATE RANGE          :" TO RANGE-DESC.
           MOVE BEG-PODATE             TO RANGE-DATE1.
           MOVE END-PODATE             TO RANGE-DATE2.
           PERFORM WRITE-DETAIL-LINE.
      *-------------------------------------------------------------
           MOVE "GROUP TOTALS               :" TO RANGE-LINE.
           MOVE GTT-ISO TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".

      *****************************************
       FORM-RESET SECTION.
      *****************************************
           MOVE EXT-CONAME-CONAME TO BWTXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWTXT-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BWTXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.

           MOVE BWTXT-FORM-FIELDS TO WR-BUF.
           MOVE BWTXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       SETUP-LINK-INFO SECTION.
      *****************************************
           MOVE BWTXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWTXT-HELP--DIR TO HELP-LINK-DIR.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWTXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       MISC-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************************
       IO-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

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

            IF BW-CONN-OPEN-SW = "N"
              MOVE "Y" TO BW-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BW_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BW1-FILE. *> SSP- BW,LN
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

      *  STORED PROCEDURE
      *
      *   SELECT BWFILE.BW_OWNBR,
      *          BWFILE.BW_SSNO,             
      *          BWFILE.BW_LNAME, 
      *          BWFILE.BW_FNAME,
      *          BWFILE.BW_CELL_PHONE,
      *          BWFILE.BW_LOANS,
      *          BWFILE.BW_PRIVACY_OPT_OUT, 
      *          ISNULL(LNFILE.LN_OWNBR, ' '), 
      *          ISNULL(LNFILE.LN_ACCTNO, 0),
      *          ISNULL(LNFILE.LN_SSNO1, 0),
      *          ISNULL(LNFILE.LN_MAKERCD1,' '),
      *          ISNULL(LNFILE.LN_SSNO2, 0),
      *          ISNULL(LNFILE.LN_MAKERCD2,' '),
      *          ISNULL(LNFILE.LN_SSNO3, 0),
      *          ISNULL(LNFILE.LN_MAKERCD3,' '),
      *          ISNULL(LNFILE.LN_SSNO4, 0),
      *          ISNULL(LNFILE.LN_MAKERCD4,''),
      *          ISNULL(LNFILE.LN_POCD,''),
      *          ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
      *          ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
      *          ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
      *          ISNULL(LNFILE.LN_CURBAL, 0) 
      *     FROM DBO.BWFILE BWFILE
      *
      *     LEFT OUTER JOIN
      *          DBO.LNFILE LNFILE
      *       ON (  BWFILE.BW_OWNBR    = LNFILE.LN_OWNBR
      *      AND    BWFILE.BW_SSNO     = LNFILE.LN_SSNO1   ) 
      *    WHERE (  BWFILE.BW_OWNBR    = @QBW1_WBEG_OWNBR  )
      * 
      *    ORDER BY BWFILE.BW_OWNBR,
      *             BWFILE.BW_SSNO


           EXEC SQL SET CONNECTION BW_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
                :BW1-RETURN-CODE = EXEC SSP_BWTXT_BWFILE_SELECT
              ( :QBW1-WBEG-OWNBR )                    
           END-EXEC.


           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT. *> EMBEDDED - BW,LN
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION BW_CONNECT END-EXEC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO 
                     :QBW-OWNBR,
                     :QBW-SSNO,             
                     :QBW-LNAME, 
                     :QBW-FNAME,
                     :QBW-CELL-PHONE,
                     :QBW-LOANS,
                     :QBW-PRIVACY-OPT-OUT, 
                     :QLN-OWNBR,  
                     :QLN-ACCTNO,
                     :QLN-SSNO1, 
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-POCD,
                     :QLN-LOANDATE,  
                     :QLN-PLDATE, 
                     :QLN-POFFDATE, 
                     :QLN-CURBAL 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BW_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC 
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BW1-CONNECTION.

           IF BW-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BW_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BW_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BW-CONN-OPEN-SW.


       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      *************************************************
       END-PROGRAM SECTION.
      *************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BWTXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
