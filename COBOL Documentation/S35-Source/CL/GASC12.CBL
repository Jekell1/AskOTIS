       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GASC12.
       DATE-WRITTEN.    081889.
      **************************************************************************
      *                 (CL)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 GEORGIA RESTRICTION TEST FOR SERCHG
      *
      *                 CALLED FROM XONPC0.C AND LONPC2.C
      *
      *  WORLD: THEY USE FORMULA 54 NOW BUT I DID NOT WANT TO REMOVE THIS
      *         PROGRAM 5/2023
      *
      * NOTE *****************************************************
      *      MUST BE LOOKED AT FOR ADDON LOANS....
      *      LNAMT IS MOVED TO LE-REC
      ************************************************************
      * REV:
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  081497 JTG ADDED TEST OF LN-GIFTNET = "Y", DONT RESTRICT
      *             BUT CREATE A DUAL LE RECORD # 95
      *  042498  CS ADDED TEST FOR RB RO WHERE PB,RN,SC IS TESTED
      *  MG  990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  JTG 010426 CHANGED RE: GEORGIA GILA CHANGE EFF: 04/13/01 WORLD PR#263
      *  BAH 130131 A30 CONVERSION
      *  BAH 170323 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0425 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED 'LE' FILE TO HAVE SUB-PREFIX OF 'E' OR 'R'.
      *
      * BAH 181203 GLOBAL LNFILE
      * BAH 20190719 ADDED CLEAR-LE-FILE (LPLECL) FUTURE FIELDS NOT SET
      * BAH 20220803 HARD CODED START
      * BAH 20230712 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
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
           "@(#)CL/GASC12.CBL S35 07/12/23-10:36:22 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".

       01  ERRCD                    PIC X             VALUE SPACES.
       01  WORKER                   PIC S9(7)V99 COMP.
       01  TAA-SUB                  PIC 9(3).
       01  TAA-SUBX                 PIC 9(3).
       01  TAA-RESTRICT-ADJ         PIC S9(7)V99.
       01  TAA-DUAL-LOAN-ADJ        PIC S9(7)V99.
       01  TAA-BW-COUNT             PIC S9.
       01  TAH-LN-OWNBR             PIC 9(4).
       01  TAH-LN-ACCTNO            PIC 9(8).
       01  TAH-RESTRICT-ADJ         PIC S9(7)V99.
       01  TAH-1ST-LP               PIC X.
       01  WAB-SUB                  PIC 9(3).
       01  XAB-SUB                  PIC 9(3).

       01  RENEWAL-LNAMT            PIC 9(6)V99       VALUE 0.
       01  ACCTNO-CNT-DUAL          PIC 999 VALUE 0.
       01  ACCTNO-CNT               PIC 99  VALUE 0.
       01  ACCTNO-FOUND             PIC X.
       01  ACCTNO-TABLE             VALUE ALL ZEROS.
           03  ACCTNO-TAB           OCCURS 40 TIMES
                                    PIC 9(8).
       01  ACCTNO-TAB-MAX           PIC 99  VALUE 40.

       01  MSEL-LIST       PIC X(5)      VALUE "MSEL.".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/PAYOFXW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/GASCHGW.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME SCHG-WORKERS EXIT-PATHNAME.

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SEARCHING FOR CURRENT AND PAIDOFF LOANS." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO SCHG-RESTRICT-ADJ SCHG-DUAL-LOAN-ADJ
                     SCHG-DUAL-LIAB    SCHG-DUAL-ACCTS.

      * TEST 1ST MAKER FOR REBATE ADJUSTMENT:
           MOVE 1 TO TAA-SUB.
           PERFORM TAA-TEST-MAKER.

      * TEST 2ND MAKER FOR REBATE ADJUSTMENT:
           MOVE 2 TO TAA-SUB.
           PERFORM TAA-TEST-MAKER.

      * TEST 3RD MAKER FOR REBATE ADJUSTMENT:
      *    MOVE 3 TO TAA-SUB.
      *    PERFORM TAA-TEST-MAKER.

      * TEST 4TH MAKER FOR REBATE ADJUSTMENT:
      *    MOVE 4 TO TAA-SUB.
      *    PERFORM TAA-TEST-MAKER.

      * CREATE LOAN ENDORSER RECORD FOR RENEWAL:
           IF SCHG-UPDATE-FG = "Y"
              IF SCHG-PRIOR-ACCTNO NOT = 0
                 PERFORM TBB-CREATE-LE-RENWL.

      * CHECK FOR NEED OF COMAKER DUAL LOAN RECORDS:
           IF SCHG-UPDATE-FG = "Y"
              IF ACCTNO-CNT-DUAL < 2
                 PERFORM WAA-CREATE-COMAKER-DUAL-RECORD.

       UPDATE-EXIT.
           GO TO END-ROUTINE.

      ***************************************************
      *    TEST MAKER FOR LOANS
      ***************************************************
       TAA-TEST-MAKER SECTION.

           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR   TO LN-OWNBR.
           MOVE SCHG-LN-SSNO(1) TO LN-SSNO(TAA-SUB).

           PERFORM TAC-START-LN-FILE.
       TAA-READ-NEXT.
           PERFORM TAD-READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO TAA-EXIT.

           MOVE 0 TO TAA-RESTRICT-ADJ TAA-DUAL-LOAN-ADJ.
           IF ( LN-POCD-NONLOAN )
              GO TO TAA-READ-NEXT.

           IF LN-SSNO(TAA-SUB) NOT = SCHG-LN-SSNO(1)
              GO TO TAA-EXIT.
           IF LN-ACCTNO = SCHG-PRIOR-ACCTNO
              MOVE LN-LNAMT TO RENEWAL-LNAMT
              GO TO TAA-READ-NEXT.
           IF LN-ACCTNO = SCHG-NEW-ACCTNO
              GO TO TAA-READ-NEXT.

           IF ( (LN-SALEFIN = "Y")
                  OR (LN-SMALLOAN NOT = "Y")
                   OR (LN-GIFTNET = "Y")
                    OR (LN-ORGST NOT = SCHG-ORGST) )
              IF LN-CURBAL NOT = 0 AND SCHG-UPDATE-FG = "Y"
                 PERFORM TBA-CREATE-LE-DUAL
                 GO TO TAA-READ-NEXT
              ELSE
                 GO TO TAA-READ-NEXT.

      * GET SSNO COUNTS FOR SCHG-SSNO() AND LN-SSNO():
           MOVE 0 TO TAA-BW-COUNT.
           PERFORM TAA-COUNT-BORROWERS VARYING TAA-SUBX
      *       FROM 1 BY 1 UNTIL TAA-SUBX > 4.
              FROM 1 BY 1 UNTIL TAA-SUBX > 2.
           IF TAA-BW-COUNT NOT = 0
              IF SCHG-UPDATE-FG = "Y"
                 IF LN-CURBAL NOT = 0
                    PERFORM TBA-CREATE-LE-DUAL
                    GO TO TAA-READ-NEXT
                 ELSE
                    PERFORM TBC-CREATE-LE-PO15
                    GO TO TAA-READ-NEXT
              ELSE
                 GO TO TAA-READ-NEXT.

      * DETERMINE IF DUAL LOAN REQUIREMENTS EXIST:
           IF SCHG-LN-SSNO(2) NOT = 0
              MOVE 2 TO TAA-SUBX
              PERFORM TAA-TEST-FOR-SSNO
              IF TAA-SUBX = 0
                 IF SCHG-UPDATE-FG = "Y"
                    IF LN-CURBAL NOT = 0
                       PERFORM TBA-CREATE-LE-DUAL
                       GO TO TAA-READ-NEXT
                    ELSE
                       PERFORM TBC-CREATE-LE-PO15
                       GO TO TAA-READ-NEXT
                 ELSE
                    GO TO TAA-READ-NEXT.
      *
      *       ELSE
      *       IF SCHG-LN-SSNO(3) NOT = 0
      *          MOVE 3 TO TAA-SUBX
      *          PERFORM TAA-TEST-FOR-SSNO
      *          IF TAA-SUBX = 0
      *             IF SCHG-UPDATE-FG = "Y"
      *                IF LN-CURBAL NOT = 0
      *                   PERFORM TBA-CREATE-LE-DUAL
      *                   GO TO TAA-READ-NEXT
      *                ELSE
      *                   PERFORM TBC-CREATE-LE-PO15
      *                   GO TO TAA-READ-NEXT
      *             ELSE
      *                GO TO TAA-READ-NEXT
      *          ELSE
      *          IF SCHG-LN-SSNO(4) NOT = 0
      *             MOVE 4 TO TAA-SUBX
      *             PERFORM TAA-TEST-FOR-SSNO
      *             IF TAA-SUBX = 0
      *                IF SCHG-UPDATE-FG = "Y"
      *                   IF LN-CURBAL NOT = 0
      *                      PERFORM TBA-CREATE-LE-DUAL
      *                      GO TO TAA-READ-NEXT
      *                   ELSE
      *                      PERFORM TBC-CREATE-LE-PO15
      *                      GO TO TAA-READ-NEXT
      *                ELSE
      *                   GO TO TAA-READ-NEXT.

      *********************************************************
      * IF A DUAL LOAN, DETERMINE;
      *
      *    DUAL-LOAN-ADJ  = CURRENT BALANCE -
      *                     MAINT. FEE REBATE + PAID AHEAD:
      *    DUAL-LOAN-LIAB = CURRENT BALANCE -
      *                     MAINT. FEE REBATE + PAID AHEAD:
      *********************************************************
           IF LN-CURBAL = 0
              GO TO TAA-TEST-PAYOFF.

           MOVE SCHG-TRANS-DATE TO PASS-TRANS-DATE.
           MOVE LN-ACCTNO TO PASS-PRLNNO.
           MOVE "PB" TO PASS-CURRENT-PD-SRCD.

           MOVE "LP/PAYOFX" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASS-PAYOFF-FLDS.
           CANCEL FORM-PROGX.

           COMPUTE TAA-DUAL-LOAN-ADJ =
                      TAA-DUAL-LOAN-ADJ + PASS-PRBAL - PASS-MF-REB.
           IF PASS-CDEL-ARREARAGE > 0
              ADD PASS-CDEL-ARREARAGE TO TAA-DUAL-LOAN-ADJ.
           ADD TAA-DUAL-LOAN-ADJ TO SCHG-DUAL-LOAN-ADJ.

           COMPUTE SCHG-DUAL-LIAB =
                      SCHG-DUAL-LIAB + PASS-PRBAL - PASS-MF-REB.
           ADD 1 TO SCHG-DUAL-ACCTS.

           IF SCHG-UPDATE-FG = "Y"
              PERFORM TBA-CREATE-LE-DUAL.

           GO TO TAA-READ-NEXT.

      *******************************************************
      * SEE IF THE LOAN WAS PAID OFF WITHIN THE LAST 15 DAYS:
      *******************************************************
       TAA-TEST-PAYOFF.
           MOVE LN-POFFDATE     TO NUM-DATE.
           MOVE SCHG-TRANS-DATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < 0 OR > 15
              GO TO TAA-15-DAY-UPDATE-TEST.

      * ACCOUNT MUST HAVE PAID OUT BEFORE MATURITY:
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO NUM-DATE.
           MOVE LN-POFFDATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS NOT < 0
              GO TO TAA-15-DAY-UPDATE-TEST.

      * TEST FOR CUSTOMER PAYOFF AND IF SO FIND
      * ALL PAYMENTS MADE IN THE LAST 15 DAYS:
           MOVE LN-OWNBR  TO TAH-LN-OWNBR.
           MOVE LN-ACCTNO TO TAH-LN-ACCTNO.
           PERFORM TAH-FIND-PAYMENTS.
           ADD LN-STATE-WORKER2 TO TAH-RESTRICT-ADJ.
           IF TAH-RESTRICT-ADJ = 0
              GO TO TAA-15-DAY-UPDATE-TEST.

      * SEE HOW LONG ACCOUNT EXISTED THRU TODAY:
           MOVE LN-LOANDATE     TO NUM-DATE.
           MOVE SCHG-TRANS-DATE TO SYS-DATE.
           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
      *   ----------------------------------------------------
      *   TEST FOR NEW GILA LAW
      *   IF LOAN UNDER CONSIDERATION FOR RESTRICTION WAS MADE
      *   ON OR PRIOR TO 041301 USE $200.00 ELSE $300.00
      *   ----------------------------------------------------
           IF LN-LOANDATE <= 20010413
              MOVE 200.00 TO WORKER
           ELSE
              MOVE 300.00 TO WORKER.
           IF TAH-RESTRICT-ADJ <= WORKER
              IF ELAPSED-DAYS < 61
                 NEXT SENTENCE
              ELSE
                 GO TO TAA-15-DAY-UPDATE-TEST
           ELSE
              IF ELAPSED-DAYS < 181
                 NEXT SENTENCE
              ELSE
                 GO TO TAA-15-DAY-UPDATE-TEST.

           MOVE TAH-RESTRICT-ADJ TO TAA-RESTRICT-ADJ.
           ADD TAH-RESTRICT-ADJ TO SCHG-RESTRICT-ADJ.

       TAA-15-DAY-UPDATE-TEST.
           IF SCHG-UPDATE-FG = "Y"
              PERFORM TBC-CREATE-LE-PO15.
           GO TO TAA-READ-NEXT.

       TAA-COUNT-BORROWERS.
           IF SCHG-LN-SSNO(TAA-SUBX) = 0
              ADD 1 TO TAA-BW-COUNT.
           IF LN-SSNO(TAA-SUBX) = 0
              SUBTRACT 1 FROM TAA-BW-COUNT.

       TAA-TEST-FOR-SSNO.
           IF SCHG-LN-SSNO(TAA-SUBX) = LN-SSNO(1)
                                   OR = LN-SSNO(2)
      *                             OR = LN-SSNO(3)
      *                              OR = LN-SSNO(4)
              NEXT SENTENCE
           ELSE
              MOVE 0 TO TAA-SUBX.

       TAA-EXIT.
           PERFORM CLOSE-LN4-FILE.

      **********************************************
      *    START LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       TAC-START-LN-FILE SECTION.
           IF TAA-SUB = 1
              MOVE LN-PATH-OWNBR    TO QLN4-WBEG-OWNBR
                                       QLN4-WEND-OWNBR
              MOVE LN-SSNO(TAA-SUB) TO QLN4-WBEG-SSNO1
                                       QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
              IF TAA-SUB = 2
                 MOVE LN-PATH-OWNBR    TO QLN5-WBEG-OWNBR
                                          QLN5-WEND-OWNBR
                 MOVE LN-SSNO(TAA-SUB) TO QLN5-WBEG-SSNO2
                                          QLN5-WEND-SSNO2
                 PERFORM START-LN5-FILE.
      *       ELSE
      *          IF TAA-SUB = 3
      *             MOVE LN-PATH-OWNBR    TO QLN6-WBEG-OWNBR
      *                                      QLN6-WEND-OWNBR
      *             MOVE LN-SSNO(TAA-SUB) TO QLN6-WBEG-SSNO3
      *                                      QLN6-WEND-SSNO3
      *             PERFORM START-LN6-FILE
      *          ELSE
      *             IF TAA-SUB = 4
      *                MOVE LN-PATH-OWNBR    TO QLN7-WBEG-OWNBR
      *                                         QLN7-WEND-OWNBR
      *                MOVE LN-SSNO(TAA-SUB) TO QLN7-WBEG-SSNO4
      *                                         QLN7-WEND-SSNO4
      *                PERFORM START-LN7-FILE.

      **********************************************
      *    READ-NEXT LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       TAD-READ-LN-FILE-NEXT SECTION.
           IF TAA-SUB = 1
              PERFORM READ-LN4-FILE-NEXT
           ELSE
              IF TAA-SUB = 2
                 PERFORM READ-LN5-FILE-NEXT.
      *       ELSE
      *          IF TAA-SUB = 3
      *             PERFORM READ-LN6-FILE-NEXT
      *          ELSE
      *             IF TAA-SUB = 4
      *                PERFORM READ-LN7-FILE-NEXT.

      **********************************************
      *    FIND TOTAL PAYMENTS MADE IN LAST 15 DAYS
      *
      *    IF ACCOUNT WAS PAID OFF BY
      *       PB, RN OR SC SKIP
      **********************************************
       TAH-FIND-PAYMENTS SECTION.
           MOVE 0 TO TAH-RESTRICT-ADJ.
           MOVE " " TO TAH-1ST-LP.

           PERFORM OPEN-LP1-FILE.

           MOVE TAH-LN-OWNBR  TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE TAH-LN-ACCTNO TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       TAH-READ-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF (IO-BAD) OR (LP-BRNO NOT = TAH-LN-OWNBR) OR
                           (LP-ACCTNO NOT = TAH-LN-ACCTNO)
              GO TO TAH-EXIT.

           IF LP-REV = "Y" OR LP-TRCD = "RV"
              GO TO TAH-READ-NEXT.

           IF TAH-1ST-LP = " "
              MOVE "Y" TO TAH-1ST-LP
              IF (LP-CURBAL NOT = 0)
               OR (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                  GO TO TAH-EXIT.
           IF NOT (LP-TRCD = "PO")
              GO TO TAH-READ-NEXT.

      * SEE IF THE PAYMENT WAS MADE WITHIN THE LAST 15 DAYS:
           MOVE LP-PAYDATE      TO NUM-DATE.
           MOVE SCHG-TRANS-DATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < 0 OR > 15
              GO TO TAH-EXIT.

           ADD LP-TRAMT TO TAH-RESTRICT-ADJ.
           GO TO TAH-READ-NEXT.

       TAH-EXIT.
           PERFORM CLOSE-LP1-FILE.

      *************************************************
      *    CREATE LEFILE DUAL LOAN RESTRICTED RECORDS
      *************************************************
       TBA-CREATE-LE-DUAL SECTION.
           PERFORM OPEN-LE1-FILE.

           PERFORM CLEAR-LE-FILE.

           MOVE LN-OWNBR        TO LE-BRNO.
           MOVE SCHG-NEW-ACCTNO TO LE-ACCTNO.
           MOVE "R"             TO LE-TYPE.
           MOVE "D"             TO LE-FLAG.
           MOVE 0               TO LE-SSNO.
           MOVE LN-ACCTNO       TO LE-OSACCTNO.

           ADD 1 TO ACCTNO-CNT-DUAL.
           MOVE LN-LOANDATE       TO LE-R-LOANDATE.
           MOVE LN-LNAMT          TO LE-R-LNAMT.
           MOVE LN-POFFDATE       TO LE-R-POFFDATE.
           MOVE TAA-DUAL-LOAN-ADJ TO LE-R-RESTRICTED-MONEY.
           MOVE LN-CURBAL         TO LE-R-BALANCE.
           MOVE LN-SALEFIN        TO LE-R-SALEFIN.

           PERFORM WRITE-LE1-FILE.
           PERFORM CLOSE-LE1-FILE.

           PERFORM XAA-UPDATE-ACCTNO-TABLE.

      ***********************************************
      *    CREATE LEFILE RENEWAL RESTRICTED RECORDS
      ***********************************************
       TBB-CREATE-LE-RENWL SECTION.
           PERFORM OPEN-LE1-FILE.

           PERFORM CLEAR-LE-FILE.

           MOVE LN-OWNBR               TO LE-BRNO.
           MOVE SCHG-NEW-ACCTNO        TO LE-ACCTNO.
           MOVE "R"                    TO LE-TYPE.
           MOVE "R"                    TO LE-FLAG.
           MOVE 0                      TO LE-SSNO.
           MOVE SCHG-PRIOR-ACCTNO      TO LE-OSACCTNO.

           MOVE RENEWAL-LNAMT          TO LE-R-LNAMT.
           MOVE SCHG-PRIOR-RESTRICT    TO LE-R-RESTRICTED-MONEY.
           MOVE SCHG-PRIOR-NET-BALANCE TO LE-R-BALANCE.
           MOVE SCHG-PRIOR-LOANDATE    TO LE-R-LOANDATE.
           MOVE SCHG-TRANS-DATE        TO LE-R-POFFDATE.
           MOVE LN-SALEFIN             TO LE-R-SALEFIN.

           PERFORM WRITE-LE1-FILE.
           PERFORM CLOSE-LE1-FILE.
           PERFORM XAA-UPDATE-ACCTNO-TABLE.

      ****************************************************
      *    CREATE LEFILE PO (15 DAYS) RESTRICTED RECORDS
      ****************************************************
       TBC-CREATE-LE-PO15 SECTION.
           PERFORM OPEN-LE1-FILE.

           PERFORM CLEAR-LE-FILE.

           MOVE LN-OWNBR         TO LE-BRNO.
           MOVE SCHG-NEW-ACCTNO  TO LE-ACCTNO.
           MOVE "R"              TO LE-TYPE.
           MOVE "P"              TO LE-FLAG.
           MOVE 0                TO LE-SSNO.
           MOVE LN-ACCTNO        TO LE-OSACCTNO.

           MOVE LN-LOANDATE      TO LE-R-LOANDATE.
           MOVE LN-LNAMT         TO LE-R-LNAMT.
           MOVE LN-POFFDATE      TO LE-R-POFFDATE.
           MOVE TAA-RESTRICT-ADJ TO LE-R-BALANCE
                                    LE-R-RESTRICTED-MONEY.
           MOVE LN-SALEFIN       TO LE-R-SALEFIN.

           PERFORM WRITE-LE1-FILE.
           PERFORM CLOSE-LE1-FILE.
           PERFORM XAA-UPDATE-ACCTNO-TABLE.

      ****************************************************
      *    CREATE COMAKER DUAL LOAN RECORDS
      ****************************************************
       WAA-CREATE-COMAKER-DUAL-RECORD SECTION.
           MOVE 0 TO TAA-DUAL-LOAN-ADJ.

           IF ACCTNO-CNT-DUAL < 2
              IF SCHG-LN-SSNO(2) NOT = 0
                 MOVE 2 TO WAB-SUB
                 MOVE 1 TO TAA-SUB
                 PERFORM WAB-TEST-MAKER
                    UNTIL TAA-SUB NOT < 2.

      *    IF ACCTNO-CNT-DUAL < 2
      *       IF SCHG-LN-SSNO(3) NOT = 0
      *          MOVE 3 TO WAB-SUB
      *          MOVE 1 TO TAA-SUB
      *          PERFORM WAB-TEST-MAKER
      *             UNTIL TAA-SUB NOT < 4.

      *    IF ACCTNO-CNT-DUAL < 2
      *       IF SCHG-LN-SSNO(4) NOT = 0
      *          MOVE 4 TO WAB-SUB
      *          MOVE 1 TO TAA-SUB
      *          PERFORM WAB-TEST-MAKER
      *             UNTIL TAA-SUB NOT < 4.

       WAA-EXIT.
           EXIT.

      ****************************************************
       WAB-TEST-MAKER SECTION.
      ****************************************************
           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR         TO LN-OWNBR.
           MOVE SCHG-LN-SSNO(WAB-SUB) TO LN-SSNO(TAA-SUB).

           PERFORM TAC-START-LN-FILE.
       WAB-READ-NEXT.
           PERFORM TAD-READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO WAB-EXIT.

           IF ( LN-POCD-NONLOAN )
              GO TO WAB-READ-NEXT.

           IF LN-SSNO(TAA-SUB) NOT = SCHG-LN-SSNO(WAB-SUB)
              GO TO WAB-EXIT.
           IF LN-ACCTNO = SCHG-NEW-ACCTNO
              GO TO WAB-READ-NEXT.

           PERFORM XAB-SEARCH-ACCTNO-TABLE.
           IF ACCTNO-FOUND = " "
              PERFORM TBA-CREATE-LE-DUAL.
           GO TO WAB-READ-NEXT.

       WAB-EXIT.
           ADD 1 TO TAA-SUB.
           PERFORM CLOSE-LN4-FILE.

      ****************************************************
      *    UPDATE ACCTNO TABLE
      ****************************************************
       XAA-UPDATE-ACCTNO-TABLE SECTION.
           IF ACCTNO-CNT < ACCTNO-TAB-MAX
              ADD 1 TO ACCTNO-CNT
              MOVE SCHG-NEW-ACCTNO TO ACCTNO-TAB(ACCTNO-CNT).

      ****************************************************
      *    SEARCH ACCTNO TABLE
      ****************************************************
       XAB-SEARCH-ACCTNO-TABLE SECTION.
           MOVE 0 TO XAB-SUB.
           MOVE " " TO ACCTNO-FOUND.

       XAB-AGAIN.
           ADD 1 TO XAB-SUB.
           IF XAB-SUB NOT > ACCTNO-TAB-MAX
             IF ACCTNO-TAB(XAB-SUB) NOT = 0
                IF LN-ACCTNO = ACCTNO-TAB(XAB-SUB)
                   MOVE "Y" TO ACCTNO-FOUND
                ELSE
                   GO TO XAB-AGAIN.

      *********************************
       MISC-ROUTINES SECTION.
      *********************************
       SETUP-LINK-INFO.
       FORM-RESET.
       VDU-CONVERT-FIELD.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLECL.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LE1-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLE1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
