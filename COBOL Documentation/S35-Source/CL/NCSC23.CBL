       IDENTIFICATION DIVISION.
       PROGRAM-ID.      NCSC23.
       DATE-WRITTEN.    043096.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 NORTH CAROLINA, (SCFRMLA 23)
      *                 NORTH CAROLINA, (SCFRMLA 43) FCF
      *                 NORTH CAROLINA, (SCFRMLA 45) HCRED
      *                 TENNESSEE (SCFRMLA 48)
      *                 ILLINOIS (SCFRMLA 50) WORLD
      *                 NSA (SCFRMLA 57)
      *
      * FLAT FEE, CAN ONLY BE CHARGE TWICE IN ANY 12 MONTH PERIOD.
      * DOES NOT MATTER IF ITS BEEN RENEWED OR NOT, NEVER MORE THAN 2
      * SERVICE CHARGES WITHIN A 12 MONTH PERIOD !
      *
      * SCFRMLA 56 ONLY (WORLD, TX)
      *  DOES NOT MATTER IF ITS BEEN RENEWED OR NOT, CAN ONLY BE CHARGED
      *  ONCE WITHIN A MONTH (30 DAYS USING 360), STATE AND CLASS MUST MATCH
      *
      * SCFRMLA 45 ONLY;
      *  SEARCHES ALL MAKERS, COMAKERS, ETC....(PR#2274, WAGNER, 04/22/04)
      *
      *                 CALLED FROM XONPC0.C AND LONPC2.C
      * SCFRMLA 48 ONLY
      *  SEARCHED PRIOR LOANS SPR TO DETERMINE IF SCFRMLA = 48
      *  INPUT: NCSCHG-LN-ACCTNO PASSED IN IS THE PRIOR LOAN NUMBER.
      *         NCSCHG-SCFRMLA IS THE NEW LOANS SCFRMLA.
      *
      * SCFRMLA 50 ONLY (WORLD, IL)
      *  MUST CHECK PRIOR LOANS SRCD TO DETERMINE IF SCHG $25 OR $50
      *  INPUT: NCSCHG-LN-ACCTNO PASSED IN IS THE PRIOR LOAN NUMBER.
      *
      *  REV:
      * 111496 KEC CHANGED LN-POCD FOR RESCISSION.
      * 103197 BAH IF ELAPSED MONTHS = 12 AND DAYS = 1, THEN THEY
      *            CAN CHARGE A SERVICE CHARGE. THE OTHER TEST WAS
      *            WAITING UNTIL 13 MONTHS HAVE ELAPSED !!
      *  MG  990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BAH 010607 ADDED SCFRMLA 43 FOR FCF "NC", PR# 1506
      * BAH 020102 ADDED SCFRMLA 45 FOR HCRED "NC", PR# 1695
      * BAH 020418 CHANGED SCFRMLA 45 TO ONLY TEST RESTRICTION ON LOANS MADE
      *            ON AND AFTER 1/6/02 , ALL NC CUSTOMERS PR#1776,1789,1785
      * BAH 040422 SEARCH ALL COMAKERS ON PREVIOUS LOANS FOR RESTRICTION
      *            IF MAKER AND COMAKER GET FLIPPED ON NEXT LOAN, IT WASNT
      *            RESTRICTING IT..ONLY SCFRMLA 45,WAGNER PR# 2274
      *  CS 060707 XONPC0 WAS NOT PASSING 2ND,3RD, OR 4TH SIGNER
      * MJD 061212 ADDED LOGIC TO TEST PRIOR LOANS SPR SCFRMLA. REGENCY PR# 2880
      *  CS 091130 CHANGE YEAR TYPE FROM 365 TO 360 IF NCSCHG-SERCHG = 45
      *                                                  REGMGM PL#669
      * BAH 110106 ADDED SCFRMLA 50 FOR WORLD, IL, NEEDS TO TEST PRIOR
      *            LOAN SRCD AND THIS WAS EASIEST PROGRAM TO CHANGE, PR# 731
      * BAH 110216 CHANGE SCFRMLA 50 TO ONLY COUNT LOANS MADE AFTER 3/21/11
      * BAH 120716 CHANGE SCFRMLA 50, WAS MISSING TRANSFERRED ACCOUNTS.
      *            DID NOT NEED TO READ 2ND PRIOR, IF FIRST PRIOR WAS A
      *            "PB" TYPE SRCD, THEN YOU KNOW YOU SHOULD CHARGE $25
      *            IF FIRST PRIOR WAS A "NB" TYPE SRCD, THEN YOU KNOW
      *            YOU SHOULD CHARGE $50. WORLD PR# 812
      * BAH 131213 ADDED SCFRMLA 56, SAME AS 6 BUT CAN ONLY CHARGE ONE
      *            SERVICE CHARGE IN A 30 DAY PERIOD. WORLD TX PR# 953
      * BAH 140123 CHANGED SCFRMLA 56 TO USE 360 YRTYPE TO CHECK
      *            30 DAYS, NO MORE THAN ONE SCHG IN A MONTH. PR# 960
      * BAH 150508 ADDED SCFRMLA 57, NO MORE THAN 2 PER 12 MONTHS, NSA PR#4747
      * BAH 170405 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181203 GLOBAL LNFILE
      * BAH 20220826 HARD CODED START
      * BAH 20230710 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      ************************************************************
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
           "@(#)CL/NCSC23.CBL S34 05/21/21-13:09:02 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       01  MSEL-LIST       PIC X(5)      VALUE "MSEL.".

       01  ERRCD           PIC X VALUE SPACES.

       01  LN-SUB          PIC 9(3) COMP-3 VALUE 0.
       01  TEST-SSNO       PIC 9(9) VALUE 0.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/NCSCHGW.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME NCSCHG-WORKERS EXIT-PATHNAME.

      ************************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SEARCHING FOR PREVIOUS LOANS....." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE " " TO NCSCHG-RESTRICTED.

           IF NCSCHG-SCFRMLA = 48
              PERFORM TEST-PRIOR-LOAN-SPR-48
              GO TO END-ROUTINE
           ELSE
           IF NCSCHG-SCFRMLA = 50
              PERFORM TEST-PRIOR-LOAN-50
              GO TO END-ROUTINE
           ELSE
              IF NCSCHG-SCFRMLA NOT = 45
                 PERFORM JUST-CHECK-MAKER
                 GO TO END-ROUTINE.

      * TEST MAKER AS ANOTHER MAKER
           MOVE 0 TO NCSCHG-NO-SERCHG.
           MOVE NCSCHG-LN-SSNO TO TEST-SSNO.
           MOVE 1 TO LN-SUB.
           PERFORM FIND-EXISTING-LOANS.
           IF NCSCHG-RESTRICTED = "Y"
      * IF ALREADY FOUND RESTRICTION, GO AHEAD AND EXIT
              GO TO END-ROUTINE.

      * TEST MAKER AS A SECOND BORROWER ON EXISTING LOAN
           MOVE 2 TO LN-SUB.
           PERFORM FIND-EXISTING-LOANS.
           IF NCSCHG-RESTRICTED = "Y"
      * IF ALREADY FOUND RESTRICTION, GO AHEAD AND EXIT
              GO TO END-ROUTINE.

      * TEST MAKER AS A THIRD BORROWER ON EXISTING LOAN
      *    MOVE 3 TO LN-SUB.
      *    PERFORM FIND-EXISTING-LOANS.
      *    IF NCSCHG-RESTRICTED = "Y"
      * IF ALREADY FOUND RESTRICTION, GO AHEAD AND EXIT
      *       GO TO END-ROUTINE.

      * TEST MAKER AS A FOURTH BORROWER ON EXISTING LOAN
      *    MOVE 4 TO LN-SUB.
      *    PERFORM FIND-EXISTING-LOANS.
      *    IF NCSCHG-RESTRICTED = "Y"
      * IF ALREADY FOUND RESTRICTION, GO AHEAD AND EXIT
      *       GO TO END-ROUTINE.

      * NOW TEST COMAKERS ON THIS LOAN, AS ANY MAKERS ON EXISTING LOANS
           IF NCSCHG-LN-SSNO-2 NOT = 0
              MOVE 0 TO NCSCHG-NO-SERCHG
              MOVE NCSCHG-LN-SSNO-2 TO TEST-SSNO
              MOVE 1 TO LN-SUB
              PERFORM FIND-EXISTING-LOANS
              IF NCSCHG-RESTRICTED = "Y"
      * IF ALREADY FOUND RESTRICTION, GO AHEAD AND EXIT
                 GO TO END-ROUTINE
              ELSE
                MOVE 2 TO LN-SUB
                PERFORM FIND-EXISTING-LOANS
                IF NCSCHG-RESTRICTED = "Y"
                   GO TO END-ROUTINE.
      *         ELSE
      *           MOVE 3 TO LN-SUB
      *           PERFORM FIND-EXISTING-LOANS
      *           IF NCSCHG-RESTRICTED = "Y"
      *              GO TO END-ROUTINE
      *           ELSE
      *             MOVE 4 TO LN-SUB
      *             PERFORM FIND-EXISTING-LOANS
      *             IF NCSCHG-RESTRICTED = "Y"
      *                GO TO END-ROUTINE.

      * NOW TEST COMAKERS ON THIS LOAN, AS ANY MAKERS ON EXISTING LOANS
      *    IF NCSCHG-LN-SSNO-3 NOT = 0
      *       MOVE 0 TO NCSCHG-NO-SERCHG
      *       MOVE NCSCHG-LN-SSNO-3 TO TEST-SSNO
      *       MOVE 1 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF NCSCHG-RESTRICTED = "Y"
      * IF ALREADY FOUND RESTRICTION, GO AHEAD AND EXIT
      *          GO TO END-ROUTINE
      *       ELSE
      *         MOVE 2 TO LN-SUB
      *         PERFORM FIND-EXISTING-LOANS
      *         IF NCSCHG-RESTRICTED = "Y"
      *            GO TO END-ROUTINE
      *         ELSE
      *           MOVE 3 TO LN-SUB
      *           PERFORM FIND-EXISTING-LOANS
      *           IF NCSCHG-RESTRICTED = "Y"
      *              GO TO END-ROUTINE
      *           ELSE
      *             MOVE 4 TO LN-SUB
      *             PERFORM FIND-EXISTING-LOANS
      *             IF NCSCHG-RESTRICTED = "Y"
      *                GO TO END-ROUTINE.

      * NOW TEST COMAKERS ON THIS LOAN, AS ANY MAKERS ON EXISTING LOANS
      *    IF NCSCHG-LN-SSNO-4 NOT = 0
      *       MOVE 0 TO NCSCHG-NO-SERCHG
      *       MOVE NCSCHG-LN-SSNO-4 TO TEST-SSNO
      *       MOVE 1 TO LN-SUB
      *       PERFORM FIND-EXISTING-LOANS
      *       IF NCSCHG-RESTRICTED = "Y"
      * IF ALREADY FOUND RESTRICTION, GO AHEAD AND EXIT
      *          GO TO END-ROUTINE
      *       ELSE
      *         MOVE 2 TO LN-SUB
      *         PERFORM FIND-EXISTING-LOANS
      *         IF NCSCHG-RESTRICTED = "Y"
      *            GO TO END-ROUTINE
      *         ELSE
      *           MOVE 3 TO LN-SUB
      *           PERFORM FIND-EXISTING-LOANS
      *           IF NCSCHG-RESTRICTED = "Y"
      *              GO TO END-ROUTINE
      *           ELSE
      *             MOVE 4 TO LN-SUB
      *             PERFORM FIND-EXISTING-LOANS
      *             IF NCSCHG-RESTRICTED = "Y"
      *                GO TO END-ROUTINE.

           GO TO END-ROUTINE.

      ****************************
      *   ALL THIS ONLY FOR LOANS AFTER 3/21/11
      * TEST PRIOR LOANS SOURCE CODE
      *     SCFRMLA 50
      * $100 SCHG ON THE FIRST LOAN (NEW)
      * $50 FOR THE FIRST PB LOAN
      * $25 FOR EACH PB THEREAFTER
      *
      *   PB 3/28/11 OF A PB FROM 3/1/10 OF A PB FROM 10/15/09 $100
      *   PB 4/1/11 OF A PB FROM 3/25/11 OF A PB FROM 2/10/10  $ 50
      *   PB 4/1/11 OF A PB FROM 3/25/11 OF A PB FROM 3/22/11  $ 25
      ****************************
       TEST-PRIOR-LOAN-50 SECTION.

           PERFORM OPEN-LN1-FILE.
      * THIS IS PRIOR LOAN NUMBER
           MOVE NCSCHG-LN-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-TEST-PRIOR-LOAN-50.

      * IF THE FIRST PB OF NEW PB IS PRIOR TO 3/21/11, CHARGE $100
      * IT IS LIKE STARTING OVER, FIRST LOAN IN THIS NEW LAW TIMEFRAME
           IF LN-LOANDATE < 20110321
              MOVE "1" TO NCSCHG-RESTRICTED
              GO TO EXIT-TEST-PRIOR-LOAN-50.

      * $100 SCHG ON THE FIRST LOAN (NEW)
      * $50 FOR THE FIRST PB LOAN
      * $25 FOR EACH PB THEREAFTER
      * NCSCHG-RESTRICTED "1" = CHARGE $100
      * NCSCHG-RESTRICTED "2" = CHARGE $50
      * NCSCHG-RESTRICTED "3" = CHARGE $25

      * HERE YOU HAVE THE FIRST PRIOR LOAN IN THE BUFFER AND IT IS NOT
      * PRIOR TO 3/21/11, SO YOU WILL AT MOST CHARGE $50
           MOVE "2" TO NCSCHG-RESTRICTED.

      * IF THIS PRIOR LOAN IS A RENEWAL, THEN THIS NEW LOAN CAN
      * ONLY CHARGE THE $25 ("3"). IF THIS PRIOR LOAN IS A "NB", THEN YOU WILL
      * EXIT HERE CHARGING $50 ("2")

      *  THIS WILL FLAG THE CALCZL PROGRAM.
           IF LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE "3" TO NCSCHG-RESTRICTED.

       EXIT-TEST-PRIOR-LOAN-50.
           PERFORM CLOSE-LN4-FILE.

      ****************************
      * TEST PRIOR LOANS SPR.  SCFRMLA = 48
      * IF THE SVCCHG IS THE SAME
      *    SET RESTRICTED FLAG SO CALCULATOR WILL
      *    EXCLUDE NEW MONIES FROM SVCCHG CALC
      ****************************
       TEST-PRIOR-LOAN-SPR-48 SECTION.

           PERFORM OPEN-LN1-FILE.
           MOVE NCSCHG-LN-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-TEST-PRIOR-LOAN-SPR-48.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.

           PERFORM OPEN-SP1-FILE
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-SP1-FILE.

           IF SP-SCFRMLA = NCSCHG-SCFRMLA
              MOVE "Y" TO NCSCHG-RESTRICTED.

       EXIT-TEST-PRIOR-LOAN-SPR-48.
           EXIT.

      ****************************
       FIND-EXISTING-LOANS SECTION.
      ****************************

           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE TEST-SSNO     TO LN-SSNO(LN-SUB).

           PERFORM START-LN-FILE.
       READ-NEXT.
           PERFORM READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO FIND-EXIT.

           IF ( LN-POCD-NONLOAN )
              GO TO READ-NEXT.

           IF LN-SSNO(LN-SUB) NOT = TEST-SSNO
              GO TO FIND-EXIT.

           IF LN-ACCTNO = NCSCHG-LN-ACCTNO
              GO TO READ-NEXT.

      * TESTING FOR ALL SERVICE CHARGES MADE WITHIN THE
      * LAST 12 MONTHS, CAN ONLY HAVE 2...
           IF LN-SERCHG = 0
              GO TO READ-NEXT.

      D    STOP MESS.
      * SCFRMLA = 45 (NC)
      * EVERY CONSUMER LOAN BOOKED ON AND AFTER 1/6/02 CAN BE CHARGED
      * 2 LOAN PROCESSING FEES, IGNORE LOANS PRIOR TO 1/6/02
           IF NCSCHG-SCFRMLA = 45
              IF LN-LOANDATE < 20020106
                 GO TO READ-NEXT.

           MOVE LN-LOANDATE       TO NUM-DATE.
           MOVE NCSCHG-TRANS-DATE TO SYS-DATE.
           IF NCSCHG-SCFRMLA = 45
              MOVE 360 TO ELAPSED-YRTYPE
           ELSE
              MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
      * 10/31/97 BAH , IF ELAPSED MONTHS = 12 AND DAYS = 1, THEN THEY
      *                CAN CHARGE A SERVICE CHARGE. THE OTHER TEST WAS
      *                WAITING UNTIL 13 MONTHS HAVE ELAPSED !!
           IF ELAPSED-MONTHS < 12
              ADD 1 TO NCSCHG-NO-SERCHG.

           GO TO READ-NEXT.

       FIND-EXIT.
           IF NCSCHG-NO-SERCHG > 1
              MOVE "Y" TO NCSCHG-RESTRICTED.
           PERFORM CLOSE-LN4-FILE.

      **********************************************
      *    START LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       START-LN-FILE SECTION.
           IF LN-SUB = 1
              MOVE LN-PATH-OWNBR TO QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR
              MOVE LN-SSNO(1)    TO QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
              IF LN-SUB = 2
                 MOVE LN-PATH-OWNBR TO QLN5-WBEG-OWNBR
                                       QLN5-WEND-OWNBR
                 MOVE LN-SSNO(2)    TO QLN5-WBEG-SSNO2
                                       QLN5-WEND-SSNO2
                 PERFORM START-LN5-FILE.
      *       ELSE
      *          IF LN-SUB = 3
      *             MOVE LN-PATH-OWNBR TO QLN6-WBEG-OWNBR
      *                                   QLN6-WEND-OWNBR
      *             MOVE LN-SSNO(3)    TO QLN6-WBEG-SSNO3
      *                                   QLN6-WEND-SSNO3
      *             PERFORM START-LN6-FILE
      *          ELSE
      *             IF LN-SUB = 4
      *                MOVE LN-PATH-OWNBR TO QLN7-WBEG-OWNBR
      *                                      QLN7-WEND-OWNBR
      *                MOVE LN-SSNO(4)    TO QLN7-WBEG-SSNO4
      *                                      QLN7-WEND-SSNO4
      *                PERFORM START-LN7-FILE.

      **********************************************
      *    READ-NEXT LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       READ-LN-FILE-NEXT SECTION.
           IF LN-SUB = 1
              PERFORM READ-LN4-FILE-NEXT
           ELSE
              IF LN-SUB = 2
                 PERFORM READ-LN5-FILE-NEXT.
      *       ELSE
      *          IF LN-SUB = 3
      *             PERFORM READ-LN6-FILE-NEXT
      *          ELSE
      *             IF LN-SUB = 4
      *                PERFORM READ-LN7-FILE-NEXT.


      *************************
       JUST-CHECK-MAKER SECTION.
      *************************
           MOVE " " TO NCSCHG-RESTRICTED.
           MOVE 0 TO NCSCHG-NO-SERCHG.

           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR  TO LN-OWNBR
                                  QLN4-WBEG-OWNBR
                                   QLN4-WEND-OWNBR.

           MOVE NCSCHG-LN-SSNO TO LN-SSNO(1)
                                  QLN4-WBEG-SSNO1
                                  QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE.
       READ-MAKER-NEXT.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO JUST-CHECK-MAKER-EXIT.

           IF ( LN-POCD-NONLOAN )
              GO TO READ-MAKER-NEXT.

           IF LN-SSNO(1) NOT = NCSCHG-LN-SSNO
              GO TO JUST-CHECK-MAKER-EXIT.

           IF LN-ACCTNO = NCSCHG-LN-ACCTNO
              GO TO READ-MAKER-NEXT.

      * TESTING FOR ALL SERVICE CHARGES MADE WITHIN THE
      * LAST 12 MONTHS, CAN ONLY HAVE 2...

           IF LN-SERCHG = 0
              GO TO READ-MAKER-NEXT.

           MOVE LN-LOANDATE       TO NUM-DATE.
           MOVE NCSCHG-TRANS-DATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           IF NCSCHG-SCFRMLA = 56
              MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

           IF NCSCHG-SCFRMLA = 56
              IF LN-CLASS NOT = NCSCHG-CLASS
                 GO TO READ-MAKER-NEXT
              ELSE
                 IF LN-ORGST NOT = NCSCHG-ORGST
                    GO TO READ-MAKER-NEXT
                 ELSE
                    IF ELAPSED-DAYS NOT > 30
      * IF YOU FOUND ONE, YOU ARE DONE, NEEDS TO BE > 1 FOR THE EXIT
                       MOVE 2 TO NCSCHG-NO-SERCHG
                       GO TO JUST-CHECK-MAKER-EXIT
                    ELSE
                       GO TO READ-MAKER-NEXT.

      *    IF ELAPSED-MONTHS NOT > 12
      * 10/31/97 BAH , IF ELAPSED MONTHS = 12 AND DAYS = 1, THEN THEY
      *                CAN CHARGE A SERVICE CHARGE. THE OTHER TEST WAS
      *                WAITING UNTIL 13 MONTHS HAVE ELAPSED !!
           IF ELAPSED-MONTHS < 12
              ADD 1 TO NCSCHG-NO-SERCHG.

           GO TO READ-MAKER-NEXT.

       JUST-CHECK-MAKER-EXIT.
           IF NCSCHG-NO-SERCHG > 1
              MOVE "Y" TO NCSCHG-RESTRICTED.

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
       COPY "LIBGB/SCREEN.CPY".

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
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE SECTION.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
