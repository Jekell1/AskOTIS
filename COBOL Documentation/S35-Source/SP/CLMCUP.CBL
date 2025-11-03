       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMCUP.
      *****************************************************************
      *                   (SP)
      *         MOTOR CLUB CLAIMS REGISTER AND UPDATE
      *         CREATED FROM A FILE RECEIVED FROM MOTOR CLUB
      *
      *         ASSURANT UPDATE (CLAIMS.C) MOVES STATS TO FIELDS
      *         IN RATING SHEET, THIS UPDATE WILL ADD SINCE IT WILL
      *         BE RUN AFTER ASSURANT.  SO IF THEY NEED TO RE-RUN
      *         RUN THIS UPDATE, THEY NEED TO FIRST RERUN ASSURANT
      *         AND MAYBE LIFE OF THE SOUTH, THEN RE-REUN THIS UPDATE!
      *         NO, THAT WAS A STUPID STATEMENT, BECAUSE PROBABLY
      *         WILL GET FEWER & FEWER CLAIMS FROM VOYAGER & IF
      *         YOU RE-RAN THIS, NEWER BRANCHES WOULD STILL GET
      *         DOUBLED UP.  SO IF YOU HAVE TO RE-RUN, WILL HAVE
      *         TO GET WRFILE FROM A BACKUP OR SOMETHING & RE-RUN
      *         CREATE NEW MONTH'S RATING SHEET, ETC.
      *
      *
      *  FILE MUST BE IN /USR/DATA/R#/ED/CLM.MC.YYMMDD RSZ ???
      *                                       (INCLUDES LF(0A))
      *    UPDATE RENAMES FILE TO .UPD
      *    REMOTE COPIES FILE (.UPD) TO RISC2 FOR REPORTING ONLY
      *
      *   CREATES GT'S BY BRANCH PER TYPE - DEBIT
      *   CREATES GT FOR 9990 BY BRANCH PER TYPE - CREDIT
      *
      * UPDATES WRFILE FOR NO CLAIMS AND $ AMOUNT
      *
      ************************************************************************
      * REV:
      * BLF 050105 FIX SYSTEM CALL TO GET CORRECT SHELL TO REMOTE COPY
      *            FILE TO RISC 2
      * BLF 050214 FIX TO SUBTRACT FROM WK-COUNT IF CLAIM-AMOUNT IS NEGATIVE,
      *            NEGATIVE CLAIMS WERE ADDING TO COUNT IN RATING SHEET FILE,
      *            WORLD PR# 379
      * BLF 050412 FIX IF F7 AFTER CLAIM DATE & REPORT-YN ENTERED, FILE
      *            WAS RENAMED & LOG REFLECTED UPDATE DONE EVEN THOUGH
      *            THERE WAS NO UPDATE, WORLD PR# 400
      * BLF 060413 CHECK IF CREATE NEW MONTHS RATING SHEET HAS BEEN RUN BY
      *            READING WRFILE; IF NO RECORDS FOR THIS MONTH, SEND A
      *            MESSAGE & GET OUT, WORLD PR# 467
      * BLF 070119 ADD UPDATE OF GT-POST-USERID, GT-POST-DATE,
      *            GT-POST-TIME, SUPPORT PR# 2780
      *  CS 140603 REPLACED MKDIR-CMD WITH C$MAKEDIR
      * KEC 2016.1201 CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BAH 170815 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2019.04.01 FIX DEBIT/CREDIT FLAG TO BE SET BASED ON GT-AMT
      *                NEGATIVE AMOUNTS WERE CREATED AS "D" DEBITS, #1366
      * BAH 20190711 ADDED GLGTCL, FUTURE FIELDS WERE NOT CLEARED!
      * BAH 20200402 REMOVED CLAIMS-R2 AND RCP-CLMCUP, NO MORE R2
      * BAH 20231207 VALIDATE IMPORT NUMERIC FIELDS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMS-FILE ASSIGN TO CLAIMS-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY CLAIMS-KEY
                  FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
      * WORK FILE FOR INVALID BRANCH/PRODUCER
           SELECT WK2-FILE ASSIGN WK2PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  FILE STATUS FILE-STAT.

       DATA DIVISION.
       FILE SECTION.
      * RSZ 129 (INCLUDING 0A)
       FD  CLAIMS-FILE
           LABEL RECORDS ARE STANDARD.
       01  CLAIMS-REC.
           03  CLAIM-WAIS                 PIC X(4).
           03  FILLER                     PIC X(4).
           03  CLAIM-TYPE                 PIC 9(3).
           03  CLAIM-AMOUNT               PIC S9(8)V99.
           03  CLAIM-NUMBER               PIC X(9).
           03  FILLER                     PIC X(13).
           03  CLAIM-LNAME                PIC X(15).
           03  CLAIM-FNAME                PIC X(10).
           03  CLAIM-CHECK-DATE-YYMMDD    PIC 9(6).
           03  CLAIM-CHECK-NO             PIC 9(8).
           03  FILLER                     PIC X(40).
           03  CLAIM-PRODUCER             PIC 9(6).
           03  CLAIM-PRODUCER-ALP REDEFINES CLAIM-PRODUCER.
               05  CLAIM-PRODUCER1        PIC X.
               05  CLAIM-BRNO             PIC 9(5).
           03  CLAIM-LF                   PIC X.

       COPY "LIBGB/GBFDPR.CPY".
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC                       PIC X(230).

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO                PIC 9(4).
               05  WK-TYPE                PIC 9(4).
           03  WK-AMOUNT                  PIC S9(7)V99 COMP-3.
           03  WK-COUNT                   PIC S9(7)    COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./SP/CLMCUP.CBL S34 06/17/21-12:34:49 >".
      *****************************************************************
      *  PATH OF DATED CLAIMS FILE (FOR MOTOR CLUB CLAIMS)
      *  FILE IS LOADED BY WORLD
      *****************************************************************
       01  CLAIMS-PATH.
           03  CLAIMS-PATH-BASE          PIC X(09).
           03  FILLER                    PIC X(01)  VALUE "/".
           03  CLAIMS-PATH-MACHINE       PIC X(02).
           03  FILLER                    PIC X(11)  VALUE "/ED/CLM.MC.".
      * YYMMDD
           03  CLAIMS-PATH-DATE          PIC 9(06).

       01  CLAIMS-UPD-PATH.
           03  CLAIMS-PATH-UPD           PIC X(29).
           03  CLAIMS-PATH-UPD-TAG       PIC X(4)   VALUE ".UPD".

       01  CLAIMS-UPDTR-PATH.
           03  CLAIMS-PATH-UPDTR         PIC X(29).
           03  CLAIMS-PATH-UPDTR-TAG     PIC X(6)   VALUE ".UPDTR".

       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LAST MONTH RATING SHEET RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING
            LEADING "WR" BY "WRLM".

       01  CLAIMS-KEY         PIC 9(7)   VALUE 1.
       01  PR-IFN             PIC X(3)   VALUE "PR".
       01  WK-IFN             PIC X(3)   VALUE "WK".
       01  WK2-IFN            PIC X(3)   VALUE "WK2".
       01  WK2PATH            PIC X(30).

       01  LN-WORKERS.
           03  LN-FEED        PIC 999   VALUE 0.
           03  LN-COUNT       PIC 999   VALUE 99.
           03  LN-WK          PIC 999   VALUE 0.
       01  PAGE-NUMBER        PIC 999   VALUE 0.

       01  HOLD-BRANCH        PIC 9(4)  VALUE 0.

       01  PROCESS-YYYYMM       PIC 9(6).
       01  LAST-MO-YYYYMM      PIC 9(6).
       01  FILLER REDEFINES LAST-MO-YYYYMM.
           03  LAST-MO-YYYY    PIC 9(4).
           03  LAST-MO-MM      PIC 99.

       77  ACCUM-TOT-DEBIT    PIC S9(10)V99 COMP-3 VALUE +0.
       77  ACCUM-TOT-CREDIT   PIC S9(10)V99 COMP-3 VALUE +0.

       77  GRAND-463-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.


       77  BRANCH-463-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.

       01  SAVE-GT-LINE       PIC 9(6)  VALUE 0.
       01  REF-WK.
           03  FILLER         PIC X(3) VALUE "LP-".
           03  REF-DATE       PIC 9(6).
           03  FILLER         PIC X(1) VALUE "-".
           03  REF-BR         PIC 9(4).

       01  FORMAT-ACCT        PIC 9(7).
       01  FILLER REDEFINES FORMAT-ACCT.
           03  ACCT-FIRST-4   PIC 9(4).
           03  ACCT-LAST-3    PIC 9(3).

       01  ERROR-FLAG         PIC X    VALUE "N".
       01  CONTINUE-UPDATE    PIC X    VALUE "Y".
       01  WK2-FLAG           PIC X    VALUE "N".
       01  FIRST-TIME         PIC X VALUE "Y".
       01  FIRST-WK           PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".

       01  MESS-LIST         PIC X(5) VALUE "MESS.".
       01  MSEL-LIST         PIC X(5) VALUE "MSEL.".
       01  LEGEND            PIC X(70).
       01  STEP-COUNT        PIC 9(4) VALUE 1.

       01  WORK-TIME          PIC 9(6).
       01  FILLER REDEFINES WORK-TIME.
           04  TIME-HHMM      PIC 9(4).
           04  TIME-SS        PIC 9(2).

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  CLAIMS-DATE     PIC 9(8).
           03  TRANS-DATE      PIC 9(8).
           03  REPORT-ONLY-FG  PIC X.

       01  VERSION-CONTROL    PIC X VALUE "A".

       01  LOG-CHANGE-MSG.
           03  LOG-BRNOX      PIC X(4).
           03  LOG-BRNO REDEFINES LOG-BRNOX
                              PIC 9(4).
           03  FILLER         PIC X     VALUE SPACES.
           03  LOG-MESS.
               05  LOG-MESS1      PIC X(25) VALUE SPACES.
               05  FILLER         PIC X(6)  VALUE "DATE: ".
               05  LOG-DATE       PIC 99/99/99.

       01  HEAD-1.
           03  HD-DATE        PIC X(8).
           03  FILLER         PIC X(3)    VALUE SPACES.
           03  HD-TIME        PIC X(8)    VALUE SPACES.
           03  FILLER         PIC X(30)   VALUE SPACES.
           03  HD-CONAME      PIC X(40).
           03  FILLER         PIC X(34)   VALUE SPACES.
           03  FILLER         PIC X(5)    VALUE "PAGE ".
           03  HD-PAGE        PIC ZZ9.

       01  HEAD-2.
           03  FILLER         PIC X(9)    VALUE "USERID:".
           03  HD-USERID      PIC X(10)   VALUE SPACES.
           03  FILLER         PIC X(35)   VALUE SPACES.
           03  FILLER         PIC X(24)   VALUE
               "MOTOR CLUB CLAIMS UPDATE".

       01  HEAD-3.
           03  FILLER        PIC X(8) VALUE "BRANCH: ".
           03  HD-BRNO       PIC Z(4).
           03  FILLER        PIC X(13) VALUE SPACES.
           03  FILLER        PIC X(37) VALUE SPACES.
           03  HD-TRANS-DATE PIC 99/99/99.

       01  HEAD-4.
           03  FILLER         PIC X(36)   VALUE SPACES.
           03  FILLER         PIC X(21)   VALUE
               "             463   ".

       01  HEAD-5.
           03  FILLER         PIC X(36)   VALUE
               "  PAID   NAME                    ".
           03  FILLER         PIC X(21)   VALUE
               "CHECK #    MOTOR CLUB".

       01  HEAD-GRAND.
           03  FILLER         PIC X(23)   VALUE SPACES.
           03  FILLER         PIC X(21)   VALUE
               "             (463)   ".

       01  HEAD-GT.
           03  FILLER        PIC X(67) VALUE "GENERAL LEDGER ENTRIES".
           03  FILLER        PIC X(10) VALUE "# CLAIMS".

       01  DETAIL-LINE        VALUE SPACES.
           03  D-DATE         PIC 99/99/99.
           03  FILLER         PIC X.
           03  D-NAME         PIC X(26).
           03  FILLER         PIC X.
           03  D-CHECK-NO     PIC ZZZ99999.
           03  FILLER         PIC X.
           03  D-MC-CLAIM     PIC ZZZZZ9.99-.
           03  FILLER         PIC X(77).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D2-BRNO         PIC ZZZ9.
           03  FILLER          PIC X(4).
           03  D2-DATE         PIC 99/99/99.
           03  FILLER          PIC X.
           03  D2-NAME         PIC X(26).
           03  FILLER          PIC X.
           03  D2-CHECK-NO     PIC 9(8).
           03  FILLER          PIC X.
           03  D2-CLAIM        PIC ZZZZZ9.99-.
           03  FILLER          PIC X.
           03  D2-TYPE         PIC 9(3).
           03  FILLER          PIC X(3).
           03  D2-DESC         PIC X(10).

       01  DETAIL-GT-LINE REDEFINES DETAIL-LINE.
           03  D-GT-BRNO      PIC 9(4).
           03  FILLER         PIC X.
           03  D-GT-ACCTNO    PIC 9(7).
           03  FILLER         PIC XX.
           03  D-GT-EXPL      PIC X(32).
           03  FILLER         PIC XX.
           03  D-GT-DRCRFG    PIC X.
           03  FILLER         PIC XX.
           03  D-GT-AMOUNT    PIC ZZZ,ZZZ,ZZ9.99-.
           03  FILLER         PIC X(3).
           03  D-GT-COUNT     PIC Z(6).
           03  FILLER         PIC X(3).
           03  D-GT-ERROR     PIC X(25).

       01  TOTAL-LINE.
           03  FILLER         PIC X(36)   VALUE SPACES.
           03  TOT-TAG        PIC X(14).
           03  TOT-AMT        PIC ZZZZ,ZZZ,ZZ9.99-.
           03  FILLER         PIC XX.

       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CLMCUP_DEF.CPY".
       COPY "LIBSP/CLMCUP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CLMCUP_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                   CLAIMS-FILE PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE CLAIMS-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***************************************************
       MAIN-PROGRAM SECTION.
      ***************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLMCUP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              MOVE "N" TO CONTINUE-UPDATE
              GO TO END-PROGRAM.

           IF REPORT-ONLY-FG = "Y"
              GO TO SKIP-WREOM1-CHECK.

           PERFORM OPEN-WR1-FILE.
           MOVE PROCESS-YYYYMM     TO WR-YYYYMM
                                      QWR1-WBEG-YYYYMM
                                      QWR1-WEND-YYYYMM.
           MOVE 0                  TO WR-BRNO
                                      QWR1-WBEG-BRNO.
           MOVE ALL "9"            TO QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE.
           PERFORM READ-WR1-FILE-NEXT.
           IF IO-FG = 9 OR (WR-YYYYMM NOT = PROCESS-YYYYMM)
              MOVE SPACES TO MESS
              MOVE CLAIMS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-DATE
              STRING "CREATE NEW MONTHS RATING SHEET NOT RUN FOR ",
                     D-DATE
                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-WR1-FILE
              MOVE "N" TO CONTINUE-UPDATE
              GO TO END-PROGRAM.

           PERFORM CLOSE-WR1-FILE.

       SKIP-WREOM1-CHECK.
           MOVE 0 TO HOLD-BRANCH.
           MOVE 0 TO CLAIMS-KEY.
           PERFORM START-CLAIMS-FILE.

       NEXT-CLAIM.
           PERFORM READ-CLAIMS-FILE-NEXT.
           IF IO-BAD
              PERFORM BRANCH-TOTALS
              GO TO PROCESS-CLAIMS.

           IF CLAIM-TYPE NOT NUMERIC OR
               CLAIM-AMOUNT NOT NUMERIC OR
                CLAIM-CHECK-DATE-YYMMDD NOT NUMERIC OR
                 CLAIM-CHECK-NO NOT NUMERIC OR
                  CLAIM-BRNO NOT NUMERIC
              MOVE "Y" TO WK2-FLAG
              MOVE CLAIMS-REC TO WK2-REC
              PERFORM WRITE-WK2-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 GO TO NEXT-CLAIM.

           IF HOLD-BRANCH = 0
              MOVE CLAIM-BRNO TO HOLD-BRANCH
           ELSE
              IF HOLD-BRANCH NOT = CLAIM-BRNO
                 PERFORM BRANCH-TOTALS
                 MOVE CLAIM-BRNO TO HOLD-BRANCH.

           MOVE CLAIM-CHECK-DATE-YYMMDD TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.

           STRING CLAIM-LNAME,", ",CLAIM-FNAME DELIMITED BY "   "
              INTO D-NAME.
           MOVE CLAIM-CHECK-NO TO D-CHECK-NO.

           IF CLAIM-TYPE = 463
              ADD CLAIM-AMOUNT TO BRANCH-463-TOTAL
              MOVE CLAIM-AMOUNT TO D-MC-CLAIM
           ELSE
              GO TO NEXT-CLAIM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE CLAIM-BRNO TO WK-BRNO.
           MOVE CLAIM-TYPE TO WK-TYPE.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE CLAIM-BRNO TO WK-BRNO
              MOVE CLAIM-TYPE TO WK-TYPE
              IF CLAIM-AMOUNT < 0
                 MOVE -1 TO WK-COUNT
              ELSE
                 MOVE 1 TO WK-COUNT
              END-IF
              MOVE CLAIM-AMOUNT TO WK-AMOUNT
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              END-IF
           ELSE
              IF CLAIM-AMOUNT < 0
                 SUBTRACT 1 FROM WK-COUNT
              ELSE
                 ADD 1 TO WK-COUNT
              END-IF
              ADD CLAIM-AMOUNT TO WK-AMOUNT
              PERFORM REWRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           IF CLAIM-BRNO = 0
              MOVE "Y" TO WK2-FLAG
              MOVE CLAIMS-REC TO WK2-REC
              PERFORM WRITE-WK2-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           GO TO NEXT-CLAIM.

       PROCESS-CLAIMS.
           PERFORM CLOSE-CLAIMS-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM CLOSE-WK2-FILE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "GRAND TOTALS" TO D-NAME.

           MOVE GRAND-463-TOTAL TO D-MC-CLAIM.

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD-GRAND TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.


           MOVE HEAD-GT TO DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.

           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-GT1-FILE.
           PERFORM OPEN-WR1-FILE.

           MOVE "Y" TO FIRST-WK.
           MOVE 0 TO WK-BRNO WK-TYPE.
           PERFORM START-WK-FILE.
       NEXT-WK-CLAIM.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-ROUTINE.

           MOVE 1 TO SAVE-GT-LINE.

           PERFORM CLEAR-GT-FILE.

           MOVE SPACES TO DETAIL-LINE.

           MOVE TRANS-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO REF-DATE.

           MOVE WK-BRNO TO REF-BR.
      * ERRORS - NEED TO GO TO BRANCH 9990 (MISSING PRODUCER #)
           IF WK-BRNO = 0
              MOVE 9990 TO REF-BR.
           MOVE REF-WK TO GT-REF.

           IF WK-TYPE = 463
              MOVE 5134 TO ACCT-FIRST-4
              MOVE "(463) MOTOR CLUB INSURANCE CLAIM" TO GT-EXPL
                                                       D-GT-EXPL.

           MOVE TRANS-DATE TO GT-DATE1 GT-DATE2 GT-DATE3.

           MOVE 3 TO GT-SOURCE.
           MOVE WK-BRNO TO GT-BRANCH GL-BRANCH D-GT-BRNO.
           MOVE 0 TO ACCT-LAST-3.
           MOVE FORMAT-ACCT TO GT-ACCTNO GL-ACCOUNT D-GT-ACCTNO.
           IF WK-BRNO = 0
              MOVE 9990 TO GT-BRANCH GL-BRANCH D-GT-BRNO
              MOVE 9999999 TO GT-ACCTNO GL-ACCOUNT D-GT-ACCTNO.

           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO ERROR-FLAG
              MOVE "** GL MASTER MISSING **" TO D-GT-ERROR.

           MOVE WK-AMOUNT TO D-GT-AMOUNT GT-AMT.

           ADD WK-AMOUNT TO ACCUM-TOT-DEBIT.
           IF GT-AMT > 0
              MOVE "1" TO GT-DRCRFG
              MOVE "D" TO D-GT-DRCRFG
           ELSE
              MOVE "2" TO GT-DRCRFG
              MOVE "C" TO D-GT-DRCRFG.

           MOVE WK-COUNT TO D-GT-COUNT.
           MOVE 0 TO GT-PERIOD.

           IF REPORT-ONLY-FG = "Y" OR GT-AMT = 0
              GO TO WRITE-REPORT-LINE.

       UPDATE-NEXT-GT1.
           PERFORM SET-POSTDATE.
           MOVE POST-DATE    TO GT-POST-DATE.
           MOVE POST-TIME    TO WORK-TIME.
           MOVE TIME-HHMM    TO GT-POST-TIME.
           MOVE POST-USERID  TO GT-POST-USERID.
           MOVE SAVE-GT-LINE TO GT-LINE.
           PERFORM WRITE-GT1-FILE.
           IF IO-BAD
              IF GT-LINE < 999999
                 ADD 1 TO SAVE-GT-LINE
                 GO TO UPDATE-NEXT-GT1.


       UPDATE-SKIP-GT.

           PERFORM UPDATE-RATING-SHEET.

       WRITE-REPORT-LINE.

           PERFORM WRITE-DETAIL-LINE.

      ******
           PERFORM UPDATE-GT-CREDIT.
      ******

           GO TO NEXT-WK-CLAIM.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "TOTAL DEBIT :" TO TOT-TAG.
           MOVE ACCUM-TOT-DEBIT  TO TOT-AMT.

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "TOTAL CREDIT:" TO TOT-TAG.
           MOVE ACCUM-TOT-CREDIT TO TOT-AMT.
           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF REPORT-ONLY-FG = "Y"
              MOVE "* * * * * * * * * * * * * * * * * * * " TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE "* * * * * REPORT ONLY OPTION * * * * *" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE "* * * * * * * * * * * * * * * * * * * " TO PR-REC
              PERFORM WRITE-PR-REC.

           IF WK2-FLAG = "Y"
              PERFORM PRINT-INVALID-RECORDS.

           MOVE SPACES TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           GO TO END-PROGRAM.


      ******************************
       PRINT-INVALID-RECORDS SECTION.
      ******************************

           MOVE SPACES TO DETAIL-LINE.
           MOVE "INVALID ACCOUNTS:" TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM OPEN-WK2-FILE.

       PRINT-NEXT-INVALID.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-PRINT-INVALID.

           MOVE WK2-REC TO CLAIMS-REC.

           MOVE CLAIM-CHECK-DATE-YYMMDD TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D2-DATE.

           STRING CLAIM-LNAME,", ",CLAIM-FNAME DELIMITED BY "   "
              INTO D2-NAME.
           MOVE CLAIM-CHECK-NO TO D2-CHECK-NO.

           MOVE CLAIM-BRNO TO D2-BRNO.

           MOVE CLAIM-AMOUNT TO D2-CLAIM.

           MOVE CLAIM-TYPE TO D2-TYPE.

           IF CLAIM-TYPE = 463
              MOVE "MOTOR CLUB" TO D2-DESC
           ELSE
              MOVE "INVALID" TO D2-DESC.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-NEXT-INVALID.


       EXIT-PRINT-INVALID.
           EXIT.

      *****************************
       UPDATE-GT-CREDIT SECTION.
      *****************************

           MOVE 1 TO SAVE-GT-LINE.

           PERFORM CLEAR-GT-FILE.

           MOVE TRANS-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO REF-DATE.

           MOVE WK-BRNO TO REF-BR.
           IF WK-BRNO = 0
              MOVE 9990 TO REF-BR.
           MOVE REF-WK TO GT-REF.

           MOVE 3501001 TO FORMAT-ACCT.
           MOVE "(463) MOTOR CLUB INSURANCE CLAIM" TO GT-EXPL
                                                    D-GT-EXPL.

           MOVE TRANS-DATE TO GT-DATE1 GT-DATE2 GT-DATE3.

           MOVE 3 TO GT-SOURCE.
           MOVE 9990 TO GT-BRANCH GL-BRANCH D-GT-BRNO.
           MOVE 0 TO GT-PERIOD.
           MOVE FORMAT-ACCT TO GT-ACCTNO GL-ACCOUNT D-GT-ACCTNO.

           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO ERROR-FLAG
              MOVE "** GL MASTER MISSING **" TO D-GT-ERROR.

           MOVE WK-AMOUNT TO D-GT-AMOUNT.
           COMPUTE GT-AMT = WK-AMOUNT * -1.
           IF GT-AMT > 0
              MOVE "1" TO GT-DRCRFG
              MOVE "D" TO D-GT-DRCRFG
           ELSE
              MOVE "2" TO GT-DRCRFG
              MOVE "C" TO D-GT-DRCRFG.

      * AD&D DOES NOT MAKE JOURNAL ENTRIES, ASSURANT PAYS THE CLAIMS
      * BUT STILL NEED TO UPDATE RATING SHEET
      * SKIP NON-FILE TOO?  AT LEAST FOR NOW, DON'T KNOW GL # BLV
           IF WK-TYPE = 459 OR 536 OR 538
              GO TO EXIT-UPDATE-GT-CREDIT.

           ADD WK-AMOUNT TO ACCUM-TOT-CREDIT.

           IF REPORT-ONLY-FG = "Y" OR GT-AMT = 0
              GO TO SKIP-UPDATE-GT-CREDIT.

       UPDATE-NEXT-CREDIT.
           PERFORM SET-POSTDATE.
           MOVE POST-DATE    TO GT-POST-DATE.
           MOVE POST-TIME    TO WORK-TIME.
           MOVE TIME-HHMM    TO GT-POST-TIME.
           MOVE POST-USERID  TO GT-POST-USERID.
           MOVE SAVE-GT-LINE TO GT-LINE.
           PERFORM WRITE-GT1-FILE.
           IF IO-BAD
              IF GT-LINE < 999999
                 ADD 1 TO SAVE-GT-LINE
                 GO TO UPDATE-NEXT-CREDIT.

       SKIP-UPDATE-GT-CREDIT.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-UPDATE-GT-CREDIT.
           EXIT.

      ********************************
       UPDATE-RATING-SHEET SECTION.
      ********************************
       UPDATE-RATING.
           IF REPORT-ONLY-FG = "Y"
              GO TO EXIT-UPDATE-RATING-SHEET.

           MOVE WK-BRNO TO WR-BRNO.
           IF WK-BRNO = 0
              MOVE 9990 TO WR-BRNO.
           MOVE PROCESS-YYYYMM TO WR-YYYYMM.

           PERFORM READ-WR1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WR-FILE
              MOVE WK-BRNO TO WR-BRNO
              IF WK-BRNO = 0
                 MOVE 9990 TO WR-BRNO
              END-IF
              MOVE PROCESS-YYYYMM TO WR-YYYYMM
              PERFORM WRITE-WR1-FILE
              IF IO-FG NOT = 0
                 GO TO EXIT-UPDATE-RATING-SHEET
              ELSE
                 GO TO UPDATE-RATING.

           PERFORM RETRIEVE-LAST-MONTH-WR.

           IF WK-TYPE = 463
              ADD WK-COUNT TO WR-NO-ROAD-GARD-CLAIMS(1)
              ADD WK-AMOUNT TO WR-ROAD-GARD-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-ROAD-GARD-CLAIMS(2)
              ADD WK-AMOUNT TO WR-ROAD-GARD-CLAIMS(2).
           PERFORM REWRITE-WR1-FILE.

       EXIT-UPDATE-RATING-SHEET.
           EXIT.

      *****************************************************************
       RETRIEVE-LAST-MONTH-WR SECTION.
      *****************************************************************

      *  READ LAST MONTHS RATING SHEET, STORE FIELDS IN WRLM-REC.
      *  RESTORE CURRENT MONTH RECORD & UPDATE END OF LAST MONTH
      *  VALUES WITH FIELDS FROM WRLM-REC

           MOVE WK-BRNO TO WR-BRNO.
           IF WK-BRNO = 0
              MOVE 9990 TO WR-BRNO.
           MOVE LAST-MO-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              PERFORM CLEAR-WR-FILE.

           MOVE WR-REC TO WRLM-REC.

           MOVE WK-BRNO TO WR-BRNO.
           IF WK-BRNO = 0
              MOVE 9990 TO WR-BRNO.
           MOVE PROCESS-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       EXIT-RETRIEVE-LAST-MO.
           EXIT.

      ************************
       BRANCH-TOTALS SECTION.
      ************************

           MOVE SPACES TO DETAIL-LINE.
           MOVE "BRANCH TOTALS" TO D-NAME.

           MOVE BRANCH-463-TOTAL TO D-MC-CLAIM.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 99 TO LN-FEED.

           ADD BRANCH-463-TOTAL TO GRAND-463-TOTAL.


           MOVE 0 TO  BRANCH-463-TOTAL.

      *******************
       HEAD-PAGE SECTION.
      *******************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.

           MOVE HEAD-1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE CLAIM-BRNO TO HD-BRNO.
           MOVE HEAD-3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD-5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

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

      ****************************
       INITIALIZATION SECTION.
      ****************************
           MOVE CLMCUP-QUEUE-POS TO Q-POS.
           MOVE CLMCUP-COPIES-POS TO C-POS.
           MOVE CLMCUP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-CLAIMS-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF REPORT-ONLY-FG = "Y"
              MOVE "REPORT IN PROGRESS..............." TO WR-BUF
           ELSE
              MOVE "UPDATE IN PROGRESS..............." TO WR-BUF.
           IF EXT-ROUTE-BUF = "00"
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF EXT-ROUTE-BUF = "00"
              IF REPORT-ONLY-FG = "N"
                 PERFORM CREATE-SPOOL-DIR
                 IF STAT NOT = "00"
                    MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENDIT
                 END-IF
                 MOVE EO-SPOOL TO PRU-FILE.

           PERFORM OPEN-PR-FILE.

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HD-TRANS-DATE.

           MOVE CLAIMS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO PROCESS-YYYYMM.

           MOVE PROCESS-YYYYMM TO LAST-MO-YYYYMM.
           IF LAST-MO-MM > 1
              SUBTRACT 1 FROM LAST-MO-MM
           ELSE
              SUBTRACT 1 FROM LAST-MO-YYYY
              MOVE 12 TO LAST-MO-MM.

           MOVE EXT-CONAME-CONAME TO HD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.

           IF REPORT-ONLY-FG NOT = "Y"
              MOVE CLAIMS-DATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO LOG-DATE
              MOVE "START MC CLAIMS UPDATE   " TO LOG-MESS1
              MOVE SPACES                      TO LOG-BRNOX
              PERFORM LOG-CHANGES.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       EO-EXEC-AUDIT SECTION.
      ****************************
           EXIT.

      ****************************
       ENTER-PARAMETERS SECTION.
      ****************************

           IF UP-KEY
              GO TO ENTER-REPORT-ONLY-YN.

        ENTER-CLAIMS-DATE.
           MOVE "ERNM060CLAIMDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY
              MOVE "N" TO CONTINUE-UPDATE
              GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENTER-CLAIMS-DATE.
           MOVE VDU-DATE-YYYYMMDD TO CLAIMS-DATE.

       VERIFY-CLAIMS-PATH.
           MOVE EXT-FILPATH-BASE     TO CLAIMS-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO CLAIMS-PATH-MACHINE.
           MOVE CLAIMS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD          TO CLAIMS-PATH-DATE.
           MOVE CLAIMS-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE "CLAIMS INPUT FILE DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CLAIMS-DATE.

           MOVE CLAIMS-PATH TO CLAIMS-PATH-UPD.
           MOVE CLAIMS-UPD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE "STOP!! CLAIMS DATE ALREADY UPDATED!!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CLAIMS-DATE.

           MOVE CLAIMS-PATH TO CLAIMS-PATH-UPDTR.
           MOVE CLAIMS-UPDTR-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE "STOP!! CLAIMS DATE ALREADY UPDATED!!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CLAIMS-DATE.

      *-----------------------------------------------------------------

        ENTER-TRANS-DATE.
           MOVE "ERNM060TRANSDATE." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-CLAIMS-DATE.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY
              MOVE "N" TO CONTINUE-UPDATE
              GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENTER-TRANS-DATE.
           MOVE VDU-DATE-YYYYMMDD TO TRANS-DATE.
           IF TRANS-DATE = 0
              GO TO ENTER-TRANS-DATE.

       ENTER-REPORT-ONLY-YN.
           MOVE "E  A010REPORTONLYFG." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-TRANS-DATE.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY
              MOVE "N" TO CONTINUE-UPDATE
              GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO ENTER-REPORT-ONLY-YN.
           MOVE VDUBUF TO REPORT-ONLY-FG.
           IF REPORT-ONLY-FG NOT = "Y"
              IF REPORT-ONLY-FG NOT = "N"
                 GO TO ENTER-REPORT-ONLY-YN.

       EXIT-PARM.
           EXIT.

      ****************************
       DISPLAY-PARAMETERS SECTION.
      ****************************
           MOVE "S  8000TRANSDATE." TO VDU.
           MOVE TRANS-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  A010REPORTONLYFG." TO VDU.
           MOVE REPORT-ONLY-FG TO VDUBUF.
           PERFORM SCREENIO.

      ***********************************************
       LOG-CHANGES SECTION.
      ***********************************************

      * LOG CHANGES TO LOG IN LG DIRECTORY
      * CODE TO WRITE CHANGE FILE.
           MOVE "W"              TO ERRLOGW-ACTION.

           MOVE LOG-BRNO         TO ERRLOGW-BRNO.
           MOVE LOG-MESS         TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *****************************************************************
       CREATE-SPOOL-DIR SECTION.
      *****************************************************************
           MOVE "WAC"      TO EO-SPOOL-PCODE.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EO-SPOOL-DATE.
           MOVE " " TO EO-SPOOL-FILE.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99
              PERFORM SYSTEM-CALL
              IF STAT NOT = "00"
                 GO TO EXIT-CREATE-SPOOL-DIR.

      *****************************************************************
      * THEY ARE ALLOWED TO RUN THIS UPDATE MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
           MOVE "/"        TO EO-SPOOL-SLASH.
           MOVE "CLMCUP"   TO EO-SPOOL-NAME.
           MOVE 1 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EO-SPOOL-STEP.
           MOVE EO-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "00" TO STAT
              GO TO EXIT-CREATE-SPOOL-DIR.
           IF STEP-COUNT < 999
              ADD 1 TO STEP-COUNT
              GO TO TRY-NEXT-STEP-NO.
       EXIT-CREATE-SPOOL-DIR.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBSP/SPWRCL.CPY".
       COPY "LIBGL/GLGTCL.CPY".
       COPY "LIBLP/POSTDATE.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CLMCUP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLMCUP-SYSDATE.
           DISPLAY CLMCUP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CLMCUP-FORM-FIELDS TO WR-BUF.
           MOVE CLMCUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CLMCUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLMCUP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CLMCUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC SECTION.
      ****************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPWR1IN.CPY".
       COPY "LIBGL/GLGT1IN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       OPEN-WK2-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK2-IFN TO E-FILE.
           OPEN OUTPUT WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE.
       OPEN-WK2-FILE.
           PERFORM OPEN-IT.
           MOVE WK2-IFN TO E-FILE.
           OPEN INPUT WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE.
       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK2-IFN TO E-FILE.
           READ WK2-FILE.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-NEXT.
       WRITE-WK2-FILE.
           PERFORM WRITE-IT.
           MOVE WK2-IFN TO E-FILE.
           WRITE WK2-REC.
           IF IO-FG = 8
              GO TO WRITE-WK2-FILE.
       CLOSE-WK2-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK2-FILE.

       OPEN-CLAIMS-FILE.
           PERFORM OPEN-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           OPEN INPUT CLAIMS-FILE.
           IF IO-FG = 8
              GO TO OPEN-CLAIMS-FILE.
           IF IO-FG = 7
              CLOSE CLAIMS-FILE
              GO TO OPEN-CLAIMS-FILE.
       READ-CLAIMS-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           MOVE CLAIMS-KEY TO E-KEYX.
           READ CLAIMS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-CLAIMS-FILE-NEXT.
       CLOSE-CLAIMS-FILE.
           PERFORM CLOSE-IT.
           CLOSE CLAIMS-FILE.
       START-CLAIMS-FILE.
           PERFORM START-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           MOVE CLAIMS-KEY TO E-KEYX.
           START CLAIMS-FILE KEY NOT < CLAIMS-KEY.
       COPY "LIBGB/FERRORS.CPY".

      **********************
       END-PROGRAM SECTION.
      **********************

           IF F7-KEY
              MOVE "N" TO CONTINUE-UPDATE.

      * RENAME CLAIMS.MMDDYY TO CLAIMS.YYMMDD.UPD
           IF REPORT-ONLY-FG = "N"
              IF CONTINUE-UPDATE = "N"
                 MOVE SPACES TO LOG-MESS
                 MOVE "MC CLAIMS UPDATE CANCELED " TO LOG-MESS1
                 MOVE SPACES                 TO LOG-BRNOX
                 PERFORM LOG-CHANGES
              ELSE
                 MOVE "MC CLAIMS UPDATE DONE " TO LOG-MESS1
                 MOVE SPACES                   TO LOG-BRNOX
                 PERFORM LOG-CHANGES
                 MOVE MV-CMD TO MV-BUF
                 MOVE CLAIMS-PATH TO MV-FILE1 MV-FILE2
                 INSPECT MV-FILE2 REPLACING FIRST "    " BY ".UPD"
                 PERFORM SYSTEM-CALL
                 IF STAT-BAD
                    MOVE "PROBLEM RENAMING CLAIMS FILE " TO MESS
                    PERFORM SEND-MESS.
      *          ELSE
      *             IF EXT-FILPATH-MACHINE = CLAIMS-R1
      *                MOVE "REL" TO GETENV-BUF
      *                PERFORM GETENV-CALL
      *                MOVE GETENV-BUF TO CLMCUP-REL CLMCUP-REL2
      *                CALL "C$TOLOWER" USING CLMCUP-EC, VALUE 3
      *                CALL "C$TOLOWER" USING CLMCUP-SHELL, VALUE 10
      *                MOVE CLAIMS-PATH-DATE TO CLMCUP-COMMAND-DATE
      *                MOVE EXT-FILPATH-FIL TO CLMCUP-COMMAND-FIL
      *                MOVE RCP-CLMCUP TO SYSTEM-BUF
      *                PERFORM SYSTEM-CALL.


           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           IF ERROR-FLAG = "Y"
              MOVE "INVALID GL ACCOUNTS EXISTS; CHECK JOURNAL!!"
                                                      TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.


       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/WRLDMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CLMCUP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
