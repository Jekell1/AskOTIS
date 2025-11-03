       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SUMMBY.
       DATE-WRITTEN.    072695.
      *************************************************************
      *   DIRECTORY  : CL
      *
      *   DESCRIPTION: CALLED PROGRAM TO ACCUMULATE PORTFOLIO
      *    TOTALS BY BRANCH FOR SALES FINANCE ( LESS SERVICE ACCTS),
      *    SERVICE ACCOUNTS AND DIRECT LOANS.
      *    ONLY LOOKS AT CURRENT NET GAIN OR MTD NEW VOLUME.
      *
      *   HOW TO USE:
      *     MOVE 'BRANCH NO'    TO SUMMBY-BRNO
      *     MOVE 'SYSTEM DATE'   TO SUMMBY-SYSDATE
      *     MOVE "CL/SUMMBY" TO FORM-NAM
      *     CALL FORM-PROGX USING FORM-PATH SUMMBY-LINK CRWKBY-REC.
      *     CANCEL FORM-PROGX
      *
      *   OUTPUT:
      *     THE CRWKBY REC IS BUILT.
      *         LEVEL 1  =  DIRECT LOAN TOTALS.
      *               2  =  SALES FINANCE (LESS SERV. ACCT) TOTALS.
      *               3  =  SERVICE ACCOUNT TOTALS.
      *
      *   COPY: LIBCL/SUMMBYW
      *
      * REV:
      *  MJD 021296 REMOVED ACCESS TO ACTIVITY TABLE BUCKETS 10, 11.
      *  MJD 050798 ALLOW ACCESS TO BUCKETS 10 AND 11. THEY ARE NOW
      *             USED FOR TRANSACTIONS "RB" AND "RO".
      *  MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 000831 FIXED GIVING SEND-MESS OF "ERROR READING GR" IN
      *             SEARCH-GR-INIT BY CHECKING WGR-ERR AFTER PERFORMING
      *             CHECK-MEMBER. THIS PROBLEM OCCURRED RUNNING CPCORP
      *             FOR 1 BRANCH & IT WAS THE LAST BRANCH & READ-NEXT
      *             ON BR LEFT HEX ZEROS IN BR-NO BECAUSE THE HIGHEST
      *             BRANCH IN BRFILE HAD BEEN DELETED
      *  BLV 041004 ADD CLASS RANGE SO THE DELINQUENCY PERCENTS ON
      *             THE COLLECTION REPORT WILL BE CALCULATED ON SELECTED
      *             CLASS RANGE, INSTEAD OF ENTIRE PORTFOLIO, ACE PR# 2374
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * BLM 160314 FIX BY DATE FOR NEW FORMAT, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      * BAH 181017 GLOBAL TYFILE
      * BAH 181018 GLOBAL RCFILE
      * BAH 181026 GLOBAL BYFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 190918 REMOVED ACCESS-CALL ON BYAFILE, BYBFILE, TYFILE AND RCFILE
      * BAH 20220830 HARD CODED START
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
           "@(#)CL/SUMMBY.CBL J35 12/21/23-08:30:32 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".


       01  PER                 PIC 9(6)  COMP VALUE 1.
       01  ACT                 PIC 9(6)  COMP VALUE 1.
       01  TYP                       PIC 9(6)  COMP VALUE 1.
       01  ACCT-SUB                  PIC 9(6)  COMP VALUE 1.
       01  FIRST-OPEN-DATE-YYYYMMDD  PIC 9(8)  VALUE 0.

       01  PAGE2-O2-TYPE  PIC 9 VALUE 1.
           88  PAGE2-REC        VALUE 1.
           88  PAGE2-NO-O2      VALUE 2.
           88  PAGE2-O2         VALUE 3.
           88  PAGE2-INT        VALUE 4.

       01  TODAYS-DATE.
           03  TODAY-MM           PIC 99.
           03  FILLER             PIC X.
           03  TODAY-DD           PIC 99.
           03  FILLER             PIC X.
           03  TODAY-YY           PIC 99.

       01  STORE-DATE               PIC 9(8).
       01  STORE-DATEX              REDEFINES STORE-DATE.
           03  STORE-CC             PIC 9(2).
           03  STORE-YR             PIC 9(2).
           03  STORE-MO             PIC 9(2).
           03  STORE-DA             PIC 9(2).

       01  BEG-BRANCH               PIC 9(4).
       01  END-BRANCH               PIC 9(4).
       01  TODAYS-BY-DATE-CCYYMM    PIC 9(6).
       01  CLASS-FOUND              PIC X     VALUE "N".

       01  GROUP-FLAG           PIC X.
       01  ENT-GROUP            PIC X(4).

       01  SV-BEG-BR                PIC 9(4)  VALUE 9999.
       01  SV-END-BR                PIC 9(4)  VALUE 9999.

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/SUMMBYW.CPY".
       COPY "LIBLP/LP01CRWKBY.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME SUMMBY-LINK CRWKBY-REC EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SUMMBY" TO FORM-PROG.

           PERFORM INITIALIZATION.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO BY-BRNO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE.

           PERFORM GET-FIRST-OPEN-DATE-YYYYMMDD.

           PERFORM OPEN-BY1-FILE.

           MOVE TODAYS-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           IF SUMMBY-CLASS-RANGE NOT NUMERIC
              MOVE 0  TO SUMMBY-BEG-CLASS
              MOVE 99 TO SUMMBY-END-CLASS.

           MOVE SUMMBY-BEG-CLASS TO BY-CLASS
                                    QBY1-WBEG-CLASS.
           MOVE SUMMBY-END-CLASS TO QBY1-WEND-CLASS.

           MOVE BY-DATE-CCYYMM   TO QBY1-WBEG-DATE-CCYYMM
                                    QBY1-WEND-DATE-CCYYMM.

           MOVE BY-BRNO TO QBY1-WBEG-BRNO
                           QBY1-WEND-BRNO.

           PERFORM START-BY1-FILE.
           PERFORM OPEN-TY1-FILE.

       NEXT-BY.
           PERFORM READ-BY1-FILE-NEXT.
           IF IO-BAD OR BY-BRNO NOT = WGR-SRESULT
              GO TO END-OF-GROUP.

           IF BY-DATE-CCYYMM NOT = TODAYS-BY-DATE-CCYYMM
              GO TO NEXT-BY.

           IF BY-CLASS > SUMMBY-END-CLASS
              GO TO NEXT-BY.

           PERFORM TEST-CLASS.
           IF CLASS-FOUND NOT = "Y"
              GO TO NEXT-BY.

       ACCUM-BY-TOTALS.
      *  DON'T GET TODAY'S TOTALS UNLESS INQUIRING ON CURRENT MONTH
           IF TODAY-MM = STORE-MO AND TODAY-YY = STORE-YR
              PERFORM GET-TODAYS-TOTALS
           ELSE
              PERFORM CLEAR-TY-FILE.

           MOVE 1 TO PER.
           PERFORM SUM-FIELDS-2-BY
               VARYING TYP FROM 1 BY 1 UNTIL TYP > 11.
           PERFORM SUM-FIELDS-2-TY
               VARYING TYP FROM 1 BY 1 UNTIL TYP > 11.
           GO TO NEXT-BY.

      *-------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-TY1-FILE.
           GO TO NEXT-GROUP-LOC.

      ********************************************************
      *    SUM FIELDS BRANCH ANALYSIS
      ********************************************************
      *  PER:
      *    PERIOD TABLE: 1 - CURRENT           1 - MTD
      *                  2 - LAST MONTH        2 - YTD
      *                  3 - LAST YEAR         3 - LYR
      *
      *       NOTE:WE ONLY NEED CUR NET AND MTD VOL TOTALS)
      *
      *  ACT:
      *    ACTIVITY TBL: 1 - NET GAIN / LOSS   2 - NEW VOLUME
      *
      *  TYP:
      *    TYPE TABLE  : 1 - NB     6 - PBI
      *                  2 - RF     7 - RN
      *                  3 - FB     8 - SC
      *                  4 - PB     9 - OT
      *                  5 - PBD   10 - RB
      *                            11 - RO
      *
      * NO OF ACCOUNTS ONLY WITH PAGE2-REC AND PAGE2-NO-O2.
      *  IF PAGE2-REC   - ACCUM RECEIVABLES WITH O2 ADDED IN.
      *  IF PAGE2-NO-O2 - ACCUM RECEIVABLES WITH O2 SUBTRACTED OUT.
      *  IF PAGE2-O2    - ACCUM O2 AMOUNTS ONLY (NO RECEIVABLES).
      *  IF PAGE2-INT   - ACCUM INTEREST CHARGES ONLY.
      *----------------------------------------------------------------
       SUM-FIELDS-2-BY SECTION.
           IF PAGE2-REC
              ADD BY-NO(PER ACT TYP) TO CRWKBY-NO(ACCT-SUB)
              ADD BY-AMT(PER ACT TYP) TO CRWKBY-AMT(ACCT-SUB)
              GO TO EXIT-SUM-FIELDS-2-BY.
           IF PAGE2-NO-O2
              ADD BY-NO(PER ACT TYP) TO CRWKBY-NO(ACCT-SUB)
              ADD BY-AMT(PER ACT TYP) TO CRWKBY-AMT(ACCT-SUB)
              SUBTRACT BY-O2(PER ACT TYP) FROM CRWKBY-AMT(ACCT-SUB)
              GO TO EXIT-SUM-FIELDS-2-BY.
           IF PAGE2-O2
              MOVE 0 TO CRWKBY-NO(ACCT-SUB)
              ADD BY-O2(PER ACT TYP) TO CRWKBY-AMT(ACCT-SUB)
              GO TO EXIT-SUM-FIELDS-2-BY.
           IF PAGE2-INT
              MOVE 0 TO CRWKBY-NO(ACCT-SUB)
              ADD BY-INT(PER ACT TYP) TO CRWKBY-AMT(ACCT-SUB)
              GO TO EXIT-SUM-FIELDS-2-BY.
       EXIT-SUM-FIELDS-2-BY.
           EXIT.

      ********************************************************
      *    SUM FIELDS TODAYS TOTALS
      ********************************************************
      *
      *  ACT:
      *    ACTIVITY TBL: 1 - NET FIXED / LOSS   2 - NEW VOLUME
      *
      *  TYP:
      *    TYPE TABLE  : 1 - NB     6 - PBI
      *                  2 - RF     7 - RN
      *                  3 - FB     8 - SC
      *                  4 - PB     9 - OT
      *                  5 - PBD   10 - RB
      *                            11 - RO
      *
      *  IF PAGE2-REC   - INCREMENT RECEIVABLES WITH O2 ADDED IN.
      *                      (CUR) - (ELM)   [O2 INCLUDED]
      *  IF PAGE2-NO-O2 - INCREMENT RECEIVABLES WITH O2 SUBTRACTED OUT.
      *                      (CUR - 02) - (ELM - O2)
      *  IF PAGE2-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *                      (CUR 02) - (ELM O2)
      *  IF PAGE2-INT   - INCREMENT INTEREST CHARGES ONLY.
      *                      (CUR INT) - (ELM INT)
      *----------------------------------------------------------------
       SUM-FIELDS-2-TY SECTION.
           IF PAGE2-REC
              ADD TY-NO(ACT TYP) TO CRWKBY-NO(ACCT-SUB)
              ADD TY-AMT(ACT TYP) TO CRWKBY-AMT(ACCT-SUB).
           IF PAGE2-NO-O2
              ADD TY-NO(ACT TYP) TO CRWKBY-NO(ACCT-SUB)
              ADD TY-AMT(ACT TYP) TO CRWKBY-AMT(ACCT-SUB)
              SUBTRACT TY-O2(ACT TYP) FROM CRWKBY-AMT(ACCT-SUB).
           IF PAGE2-O2
              ADD TY-O2(ACT TYP) TO CRWKBY-AMT(ACCT-SUB).
           IF PAGE2-INT
              ADD TY-INT(ACT TYP) TO CRWKBY-AMT(ACCT-SUB).
       EXIT-SUM-FIELDS-2-TY.
           EXIT.


      *************************************
      *    GET TODAYS TOTALS
      *************************************
       GET-TODAYS-TOTALS SECTION.
           MOVE BY-BRNO                  TO TY-BRNO.
           MOVE BY-CLASS                 TO TY-CLASS.
           MOVE FIRST-OPEN-DATE-YYYYMMDD TO TY-DATE.
           PERFORM READ-TY1-FILE.
           IF IO-BAD OR TY-BRFG = "Y"
              PERFORM CLEAR-TY-FILE.

       GET-TODAYS-TOTALS-EXIT.
           EXIT.

      ***************************************************
      *TEST CLASS IF SALES FINANCE OR LOAN CLASSES SELECTED
      ***************************************************
       TEST-CLASS SECTION.
           MOVE BY-CLASS TO LC-CODE.
           MOVE "N" TO CLASS-FOUND.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-BAD
              GO TO EXIT-TEST-CLASS.
           IF LC-SALEFIN = "Y"
              IF LC-SERVACCT = "Y"
                 MOVE 3 TO ACCT-SUB
                 MOVE "Y" TO CLASS-FOUND
              ELSE
                 MOVE 2 TO ACCT-SUB
                 MOVE "Y" TO CLASS-FOUND
           ELSE
              MOVE 1 TO ACCT-SUB
              MOVE "Y" TO CLASS-FOUND.
       EXIT-TEST-CLASS.
           EXIT.

      *********************************************************
       CLEAR-CRWKBY-REC SECTION.
      *********************************************************
           MOVE 0 TO CRWKBY-NO(1).
           MOVE 0 TO CRWKBY-NO(2).
           MOVE 0 TO CRWKBY-NO(3).
           MOVE 0 TO CRWKBY-AMT(1).
           MOVE 0 TO CRWKBY-AMT(2).
           MOVE 0 TO CRWKBY-AMT(3).
       EXIT-CLEAR-CRWKBY-REC.
           EXIT.

      *********************************************************
       INITIALIZATION SECTION.
      *********************************************************

           MOVE SUMMBY-BRNO TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR = 0
              IF WGR-TYPE = "A"
                 MOVE "Y" TO GROUP-FLAG
                 MOVE 0 TO BEG-BRANCH
                 MOVE 9999 TO END-BRANCH
                 GO TO CONT-INIT.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH END-BRANCH.

       CONT-INIT.
           MOVE BEG-BRANCH  TO BY-BRNO.
           MOVE SUMMBY-DATE TO STORE-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM TODAYS-BY-DATE-CCYYMM.
           MOVE 0           TO BY-CLASS.

           MOVE SUMMBY-TYPE TO PAGE2-O2-TYPE.

           MOVE EXT-CONAME-SYSDATE TO TODAYS-DATE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM CLEAR-CRWKBY-REC.

      *************************************
      *    GET FIRST OPEN DATE FOR BRANCH
      *************************************
       GET-FIRST-OPEN-DATE-YYYYMMDD SECTION.
           MOVE 0 TO FIRST-OPEN-DATE-YYYYMMDD.

           PERFORM OPEN-RC2-FILE.

           MOVE "A1"     TO RC-STATUS
                            QRC2-WBEG-STATUS
                            QRC2-WEND-STATUS.
           MOVE BR-NO    TO RC-BRNO
                            QRC2-WBEG-BRNO
                            QRC2-WEND-BRNO.
           MOVE 19000101 TO RC-TRANS-DATE
                            SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WBEG-TRANS-DATE.
           MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.
           PERFORM READ-RC2-FILE-NEXT.
           IF RC-BRNO = BR-NO
              IF RC-STATUS = "A1"
                 MOVE RC-TRANS-DATE TO FIRST-OPEN-DATE-YYYYMMDD.

           PERFORM CLOSE-RC2-FILE.
       EXIT-GET-FIRST-OPEN.
           EXIT.

      **************************************
      *     VARIOUS COPY MEMBERS
      ************************************
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *********************************************************
       IO-ROUTINES SECTION.
      *********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPBY1RN.CPY".
       COPY "LIBLP/LPTY1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.


       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           EXIT PROGRAM.
