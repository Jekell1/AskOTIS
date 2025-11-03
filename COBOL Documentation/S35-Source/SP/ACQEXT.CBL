       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ACQEXT.
       DATE-WRITTEN.    04/22/2019.
      *****************************************************
      *  DESC: CREATE ASCII OUTPUT FILE FOR BALANCING ACQUISITIONS
      *
      * CREATES USR/DATA/R?/FTP/ACQUISITIONS/ACQEXT.BBBB
      *                                BBBB = ENDING BRANCH
      *   PIPE DELIMITED ASCII
      *
      * REV:
      *  190422 BAH #1365B
      *  BAH 20191002 REMOVED ACCESS-CALL LNFILE
      * 02/07/2020 BAH ADDED HEADINGS, CHANGED DATES TO MM/DD/YYYY,
      *                AND ADDED WK-DIFF-PAYOFF, WK-DIFF-DUE-DAYS,
      *                WK-DIFF-APR, #1419
      * 03/11/2020 BAH SIGN WK-DIFF-PAYOFF, IF THEIR PAYOFF IS > OUR PAYOFF,
      *                DISPLAY DIFF PAYOFF AS NEG AMT. IF THEIR PAYOFF IS <
      *                OUR PAYOFF, DISPLAY DIFF PAYOFF POS AMT #1419A
      * BAH 20200831 REMOVED RESET ON BR1 
      * 09/22/2020 BAH SIGN WK-DIFF-DUE-DAYS #1462
      * BAH 20220812 HARD CODED START
      * BAH 20240909 WHILE TESTING AN ACQUISITION, NOTICED THE PLAMT WAS 
      *              MISSING IN THE EXCEL HEADER S35Q-124
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-BYTE               PIC X OCCURS 2000 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/ACQEXT.CBL S34 06/15/21-14:02:40 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".

       01  WK-PATH.
           03  WK-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  WK-PATH-MACHINE       PIC X(02).
           03  WK-PATH-DIR-FTP       PIC X(18) 
                                     VALUE "/FTP/ACQUISITIONS/".
           03  FILLER                PIC X(07)   VALUE "ACQEXT.".
           03  WK-PATH-BRNO          PIC 9(4).

       01  PIPE-1                   PIC X VALUE X"7C".
       01  WS-AMOUNT                PIC S9(7)V99   COMP-3.
       01  WS-APR                   PIC S9(4)V9(3) COMP-3.

       01  WORK-REC.
           03  WK-DLPOOL            PIC 9(5).
           03  WK-OWNBR             PIC 9(4).
           03  WK-NUMBER            PIC 9(6).
           03  WK-COLLID            PIC X(20).
           03  WK-LNAME             PIC X(16).
           03  WK-LAST4-SSNO        PIC 9(4).
           03  WK-ORGST             PIC X(2).
           03  WK-CLASS             PIC 9(2).
           03  WK-SUBCLASS          PIC 9(2).
           03  WK-LAWCODE           PIC 9(2).
           03  WK-LNAMT             PIC 9(6).99.
           03  WK-LOANDATE          PIC 99/99/9999.
           03  WK-CURBAL            PIC 9(6).99.
           03  WK-PLDATE            PIC 99/99/9999.
           03  WK-PLAMT             PIC 9(6).99.
           03  WK-TDUEPAYOFF        PIC 9(6).99.
           03  WK-PRBAL             PIC 9(6).99.
           03  WK-DIFF-PAYOFF       PIC 9(6).99-.
           03  WK-TRUEDUE-DATE      PIC 99/99/9999.
           03  WK-LASTSFORMDATE     PIC 99/99/9999.
           03  WK-DIFF-DUE-DAYS     PIC 9(3)-.
           03  WK-APRATE            PIC 999.999.
           03  WK-CONTRRATE         PIC 999.999.
           03  WK-DIFF-APR          PIC 999.999.
           03  WK-ERNDELQ-STOP-CD   PIC X.
           03  WK-UNEARN-INT        PIC 9(6).99.
           03  WK-UNEARN-MFEE       PIC 9(6).99.
           03  WK-UNEARN-SCHG       PIC 9(6).99.
           03  WK-UNEARN-DISC       PIC 9(6).99.

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  WK-IFN              PIC X(2)   VALUE "WK".

       01  FIRST-TIME          PIC X      VALUE "Y".

       01  LN-WORKERS.
           03  LN-FEED         PIC 99     VALUE 0.
           03  LN-COUNT        PIC 999    VALUE 99.
           03  LN-WK           PIC 999    VALUE 0.
       01  PAGE-NUMBER         PIC 9(4)   VALUE 0.

       01  EXCEL-HEADERS.
           03  HEAD-1     PIC X(20) VALUE "LN-DLPOOL".
           03  HEAD-2     PIC X(20) VALUE "LN-OWNBR".
           03  HEAD-3     PIC X(20) VALUE "LN-NUMBER".
           03  HEAD-4     PIC X(20) VALUE "LN-COLLID".
           03  HEAD-5     PIC X(20) VALUE "BW-LNAME".
           03  HEAD-6     PIC X(20) VALUE "LN-LAST4SNNO".
           03  HEAD-7     PIC X(20) VALUE "LN-ORGST".
           03  HEAD-8     PIC X(20) VALUE "LN-CLASS".
           03  HEAD-9     PIC X(20) VALUE "LN-SUBCLASS".
           03  HEAD-10    PIC X(20) VALUE "LN-LAWCODE".
           03  HEAD-11    PIC X(20) VALUE "LN-LNAMT".
           03  HEAD-12    PIC X(20) VALUE "LN-LOANDATE".
           03  HEAD-13    PIC X(20) VALUE "LN-CURBAL".
           03  HEAD-14    PIC X(20) VALUE "LN-PLDATE".
           03  HEAD-15    PIC X(20) VALUE "LN-PLAMT".
           03  HEAD-16    PIC X(20) VALUE "OUR PAYOFF".
           03  HEAD-17    PIC X(20) VALUE "THEIR PAYOFF".
           03  HEAD-18    PIC X(20) VALUE "DIFF PAYOFF".
           03  HEAD-19    PIC X(20) VALUE "OUR DUE".
           03  HEAD-20    PIC X(20) VALUE "THEIR DUE".
           03  HEAD-21    PIC X(20) VALUE "DIFF DUE".
           03  HEAD-22    PIC X(20) VALUE "OUR APR".
           03  HEAD-23    PIC X(20) VALUE "THEIR APR".
           03  HEAD-24    PIC X(20) VALUE "DIFF APR".
           03  HEAD-25    PIC X(20) VALUE "LN-ERNDELQ-STOP-CD".
           03  HEAD-26    PIC X(25) VALUE "COMPUTED UNEARNED INT".
           03  HEAD-27    PIC X(25) VALUE "COMPUTED UNEARNED MF".
           03  HEAD-28    PIC X(25) VALUE "COMPUTED UNEARNED SER".
           03  HEAD-29    PIC X(25) VALUE "COMPUTED UNEARNED DIS".
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)     VALUE " ".
           03  FILLER           PIC X(45)    VALUE
           "   - - ACQUISITION BALANCING EXTRACT - -".

       01  HEAD3.
           03  FILLER           PIC X(31)    VALUE
           "BRNO      TOTAL ".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  D-LINE REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(3).
           03  D-COUNT          PIC Z(9).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  FILLER           PIC X(2).
           03  RANGE2N          PIC 9(10).

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR           PIC 9(4).
           03  END-BR           PIC 9(4).
           03  BEG-CL           PIC 9(3).
           03  END-CL           PIC 9(3).
           03  BEG-ENTDATE      PIC 9(8).
           03  END-ENTDATE      PIC 9(8).
           03  AGE-DATE         PIC 9(8).
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).

       01  VERSION-CONTROL      PIC X VALUE "A".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(34) VALUE SPACES.

       01  BRANCH-COUNT         PIC 9(6)      VALUE 0.
       01  GRAND-COUNT          PIC 9(6)      VALUE 0.

       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ACQEXT_DEF.CPY".
       COPY "LIBSP/ACQEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ACQEXT_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ACQEXT" TO VDU-FORM-NAME.

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
                              BW-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE LN-OWNBR TO DISPLAY-BRNO
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE MESS-LIST   TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BR-NO  TO LN-OWNBR
                          QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR.
           MOVE BEG-CL TO LN-CLASS
                          QLN3-WBEG-CLASS.
           MOVE END-CL TO QLN3-WEND-CLASS.
           MOVE 0      TO LN-ACCTNO
                          QLN3-WBEG-ACCTNO.
           MOVE ALL "9" TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-POCD = "**"
              GO TO NEXT-LN.

           IF LN-OWNBR > END-BR
              GO TO END-OF-GROUP.

           IF LN-CLASS < BEG-CL OR LN-CLASS > END-CL
              GO TO NEXT-LN.

           IF LN-ENTDATE < BEG-ENTDATE OR LN-ENTDATE > END-ENTDATE
              GO TO NEXT-LN.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              GO TO NEXT-LN.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-LN.

      * CREATE NOTICE , USES DATE ENTERED SO DIDNT CHANGE TO READ
     
           MOVE LN-ACCTNO      TO CRNOR2-ACCTNO
           MOVE AGE-DATE       TO CRNOR2-SYSDATE
           MOVE "PO"           TO CRNOR2-LPTRCD
           MOVE "N"            TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2"    TO FORM-NAM
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC
           CANCEL FORM-PROGX.

           IF FIRST-TIME = "Y"
              PERFORM CREATE-EXCEL-HEADER
              MOVE "N" TO FIRST-TIME.

           MOVE LN-DLPOOL   TO WK-DLPOOL.
           MOVE LN-OWNBR    TO WK-OWNBR.
           MOVE LN-ACCTNO   TO WK-NUMBER.
           MOVE LN-COLLID   TO WK-COLLID.
           MOVE BW-LNAME    TO WK-LNAME.
           MOVE BW-SSNO     TO WK-LAST4-SSNO.
           MOVE LN-ORGST    TO WK-ORGST.
           MOVE LN-CLASS    TO WK-CLASS.
           MOVE LN-SUBCLASS TO WK-SUBCLASS.
           MOVE LN-LAWCODE  TO WK-LAWCODE.
           MOVE LN-LNAMT    TO WK-LNAMT.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WK-LOANDATE.

           MOVE LN-CURBAL   TO WK-CURBAL.

           MOVE LN-PLDATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WK-PLDATE.

           MOVE LN-PLAMT    TO WK-PLAMT.

      * OUR PAYOFF
           MOVE NO-TDUEPOFF TO WK-TDUEPAYOFF.
      * THEIR PAYOFF
           MOVE LN-PRBAL TO WK-PRBAL.

      * DIFF PAYOFF - IF THEIR PAYOFF IS > OUR PAYOFF, DISPLAY DIFF PAYOFF
      * AS NEGATIVE AMOUNT. IF THEIR PAYOFF IS < OUR PAYOFF, DISPLAY DIFF
      * PAYOFF AS POSITIVE AMOUNT. (3/11/2020)

           COMPUTE WS-AMOUNT ROUNDED = NO-TDUEPOFF - LN-PRBAL.
           MOVE WS-AMOUNT TO WK-DIFF-PAYOFF.

      * OUR DUE DATE
           MOVE NO-TRUE-DUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WK-TRUEDUE-DATE.
      * THEIR DUE DATE
           MOVE LN-LASTSFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WK-LASTSFORMDATE
      * DIFF DUE DAYS
           MOVE NO-TRUE-DUE-DATE TO NUM-DATE.
           MOVE LN-LASTSFORMDATE TO SYS-DATE.
           PERFORM TIM360.
           MOVE ELAPSED-DAYS TO WK-DIFF-DUE-DAYS.

      * OUR APR
           MOVE LN-APRATE        TO WK-APRATE.
      * THEIR APR
           MOVE LN-CONRATE       TO WK-CONTRRATE.
      * DIFF APR
           COMPUTE WS-APR ROUNDED = LN-APRATE - LN-CONRATE.
           MOVE WS-APR TO WK-DIFF-APR.

           MOVE LN-ERNDELQ-STOP  TO WK-ERNDELQ-STOP-CD.

           COMPUTE WS-AMOUNT ROUNDED = LN-ANTICERN(1) -
                                       LN-ACCERN(1).
           MOVE WS-AMOUNT TO WK-UNEARN-INT.

           COMPUTE WS-AMOUNT ROUNDED = LN-ANTICERN(4) -
                                       LN-ACCERN(4).
           MOVE WS-AMOUNT TO WK-UNEARN-MFEE.

           COMPUTE WS-AMOUNT ROUNDED = LN-ANTICERN(3) -
                                       LN-ACCERN(3).
           MOVE WS-AMOUNT TO WK-UNEARN-SCHG.

           COMPUTE WS-AMOUNT ROUNDED = LN-ANTICERN(5) -
                                       LN-ACCERN(5).
           MOVE WS-AMOUNT TO WK-UNEARN-DISC.


           MOVE SPACES            TO WK-REC.

           STRING WK-DLPOOL, PIPE-1,
                  WK-OWNBR, PIPE-1,
                  WK-NUMBER, PIPE-1,
                  WK-COLLID, PIPE-1,
                  WK-LNAME, PIPE-1,
                  WK-LAST4-SSNO, PIPE-1,
                  WK-ORGST, PIPE-1,
                  WK-CLASS, PIPE-1,
                  WK-SUBCLASS, PIPE-1,
                  WK-LAWCODE, PIPE-1,
                  WK-LNAMT, PIPE-1,
                  WK-LOANDATE, PIPE-1,
                  WK-CURBAL, PIPE-1,
                  WK-PLDATE, PIPE-1,
                  WK-PLAMT, PIPE-1,
                  WK-TDUEPAYOFF, PIPE-1,
                  WK-PRBAL, PIPE-1,
                  WK-DIFF-PAYOFF, PIPE-1,
                  WK-TRUEDUE-DATE, PIPE-1,
                  WK-LASTSFORMDATE, PIPE-1,
                  WK-DIFF-DUE-DAYS, PIPE-1,
                  WK-APRATE, PIPE-1,
                  WK-CONTRRATE, PIPE-1,
                  WK-DIFF-APR, PIPE-1,
                  WK-ERNDELQ-STOP-CD, PIPE-1,
                  WK-UNEARN-INT, PIPE-1,
                  WK-UNEARN-MFEE, PIPE-1,
                  WK-UNEARN-SCHG, PIPE-1,
                  WK-UNEARN-DISC, PIPE-1,
           DELIMITED BY "  " INTO WK-REC.

           ADD 1 TO BRANCH-COUNT GRAND-COUNT.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.

           IF BRANCH-COUNT NOT = 0
              MOVE BR-NO TO D-BRNO
              MOVE BRANCH-COUNT TO D-COUNT
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO BRANCH-COUNT.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           MOVE GRAND-COUNT TO D-COUNT.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PRINT-RANGE-LINES.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO END-PROGRAM.

      *******************************************
       INITIALIZATION SECTION.
      *******************************************
           MOVE ACQEXT-QUEUE-POS TO Q-POS.
           MOVE ACQEXT-COPIES-POS TO C-POS.
           MOVE ACQEXT-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "FILE CREATE IN PROCESS" TO WR-BUF
              MOVE MESS-LIST                TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE 0 TO BRANCH-COUNT GRAND-COUNT.

      * FTP NEEDS TO BE LITTLE LETTERS
           CALL "C$TOLOWER" USING WK-PATH-DIR-FTP, VALUE 18.
           MOVE EXT-FILPATH-BASE        TO WK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE     TO WK-PATH-MACHINE.
           MOVE END-BR                  TO WK-PATH-BRNO.
           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT  = "00"
              MOVE "FILE ALREADY EXISTS" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *******************************************
       ENTER-PARAMETERS SECTION.
      *******************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO CL-02.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
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
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.
       CL-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL > END-CL
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO BEG-DATE.
       ALL-CL.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO EXIT-PARM.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-ENTDATE.
       END-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO EXIT-PARM.

           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEG-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-ENTDATE.

           IF END-ENTDATE < BEG-ENTDATE
              MOVE "END DATE CANNOT BE PRIOR TO BEGINNING DATE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-DATE.

       ENTER-AGE-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO EXIT-PARM.
           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO END-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-AGE-DATE.
           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

       EXIT-PARM.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-ENTDATE.
           MOVE EXT-EOBUF-DATE2 TO END-ENTDATE AGE-DATE.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
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

           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CL TO VDUNUM0.
           PERFORM SCREENIO.

      ****************************
       CREATE-EXCEL-HEADER SECTION.
      ****************************
           MOVE SPACES TO WK-REC.

           STRING HEAD-1,PIPE-1,HEAD-2,PIPE-1,
                  HEAD-3,PIPE-1,HEAD-4,PIPE-1,
                  HEAD-5,PIPE-1,HEAD-6,PIPE-1,
                  HEAD-7,PIPE-1,HEAD-8,PIPE-1,
                  HEAD-9,PIPE-1,HEAD-10,PIPE-1,
                  HEAD-11,PIPE-1,HEAD-12,PIPE-1,
                  HEAD-13,PIPE-1,HEAD-14,PIPE-1,
                  HEAD-15,PIPE-1,HEAD-16,PIPE-1,
                  HEAD-17,PIPE-1,HEAD-18,PIPE-1,
                  HEAD-19,PIPE-1, HEAD-20,PIPE-1,
                  HEAD-21,PIPE-1,HEAD-22,PIPE-1,
                  HEAD-23, PIPE-1,HEAD-24,PIPE-1,
                  HEAD-25,PIPE-1,HEAD-26,PIPE-1,
                  HEAD-27,PIPE-1,HEAD-28,PIPE-1
           DELIMITED BY "  " INTO WK-REC.

           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      ****************************************
       WRITE-DETAIL-LINE SECTION.
      ****************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       HEAD-PAGE SECTION.
      ****************************
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

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CLASS - END CLASS" TO RANGE-SELECTION.
           MOVE BEG-CL TO RANGE1N
           MOVE END-CL TO RANGE2N
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG DATE - END DATE" TO RANGE-SELECTION.
           MOVE BEG-ENTDATE TO RANGE1N.
           MOVE END-ENTDATE TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AGE DATE" TO RANGE-SELECTION.
           MOVE AGE-DATE TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO ACQEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ACQEXT-SYSDATE.
           DISPLAY ACQEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ACQEXT-FORM-FIELDS TO WR-BUF.
           MOVE ACQEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE ACQEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE ACQEXT-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ACQEXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC-ROUTINES SECTION.
      *********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE WK-PATH TO E-FILE.
           WRITE WK-REC.
           IF IO-FG = 8
             GO TO WRITE-WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ACQEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
