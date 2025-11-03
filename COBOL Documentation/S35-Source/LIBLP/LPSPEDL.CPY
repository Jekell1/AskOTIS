      * COPYMEMBER: LIBLP/LPSPEDL
      ***********************************************************************
      *
      *  NAME:  LPSPEDL
      *  DESC:  VALIDATES:
      *            LENGTH OF TERM
      *            EFFECTIVE AND APR RATES
      *            PRINCIPAL
      *            TOTAL NOTE
      *            CASH ADVANCE
      *            SPECIAL CASH ADVANCE
      *            AMOUNTS
      *            STEP TERMS
      *            SERVICE CHARGE
      *            MAINTENANCE FEE
      *            INSURANCE PREMIUM
      *            INSURANCE COVERAGE
      *            INSURANCE TERM
      *            INSURANCE COVERAGE/BENEFIT/TERM BY AGE
      *            EXTENSION DATE USING THE MINIMUM/MAXIMUM
      *            COLLATERAL FIELDS USING EDIT TABLE IN SPR
      *
      *         N O T E:
      *                 IF THIS ROUTINE IS CHANGED ALSO CHANGE
      *                          [ LPSPEDP ]
      *                          [ LPSPEDA ]
      *
      *  IN  :  VERIFY-TODAY-DATE
      *         VERIFY-TERMCEIL-APR
      *         VERIFY-TERMCEIL-EFF-FG     "Y" -  TESTS EFFECTIVE RATE
      *                                    "N" -  NO TEST
      *         VERIFY-TERMCEIL-EFF
      *         VERIFY-AGE
      *         VERIFY-REFDATE
      *         SPTEST-CD-SECURE
      *         VERIFY-REJECT-TERM-FG      "N" -  NORMAL VERIFY
      *                                    "Y" -  SETUP TERM AGE LIMIT TABLE
      *
      *  OUT :  VALID-LOAN (Y OR N)
      *
      *  USED:  LNFILE
      *         LPAMTS
      *         TIMALL
      *         LPSPEDW
      *         LPAMTSW
      *         LPBAGE
      *
      *  COPIED BY: CL/LNVERI, CL/SFVERI, LP/LONIJ0, LP/LONIN0, LP/XONIN0
      *
      * REV :
      *  081789 BAH USE INT-CHARGEABLE FOR RATE TABLE
      *             CHANGE PRINCIPLE TO DISCLOSED-FINAMT
      *  021290 BAH ADDED PP2 INSURANCE INTO LEVEL 88'S ON INSTYPES
      *  082291 BAH ADDED AMTYPE "A" ADJ-PRINCIPAL FOR MI
      *  050592 JTG CHANGED TO EDIT INS COV RE: AGE/BENEFT TBL
      *  051292 JTG ADDED LOGIC FOR SP-INS-O1-EDIT/SP-INS-O2-EDIT (COSMO CA)
      *  021595 BAH ADDED VERIFY-PP-INSURANCE FOR WDT, INSEXCEPT 10
      *             DUAL PROP CANT BE WRITTEN IF LNAMT < 500$
      *  070795 SLC ADDED CODE FOR SP-INSREJECT-FG VALUES A AND B.
      *  071895 BAH ADDED OR IF LN-INSURED = "N" TO INS-TEST BECAUSE
      *             THE INSREJECT-FG WAS DROPPING TO $ SIGN, INVALID SUBSCRIPT!
      *  101295 BAH ADDED TEST OF LN-INSOURS = 'Y' FOR ALL INSURANCE EDITS
      *  110995 BAH ADDED TEST OF BAGE-TEST, BECAUSE BAGE-AGE WAS GETTING
      *             ADDED TO FOR EVERY INSURANCE TYPE, CAUSING TROUBLE!!
      *  011696 BAH SPTEST-INS-VALID WAS NOT SETTING SPT1&2 FOR O1 INSURANCE
      *  061097 BAH ADDED SET OF BAGE-TEST FOR SP-INSREJECT-FG = " "
      *  JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *             NO OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      *             ALSO, CHANGED LOGIC TO NOT REJECT THE LOAN BASED ON THE
      *             BORROWER'S AGE WHEN SP-INSREJECT-FG() = " "
      *  BLV 010809 CHANGED ALL PERFORMS OF SEND-MESS TO PERFORM
      *             CONDITIONAL-SEND-MESS, SO IF SFVERI CALLED FROM LNCREA,
      *             SEND-MESS WILL NOT STOP BATCH LOAN UPDATE;  IF YOU GET
      *             ERRORS FOR MISSING CONDITIONAL-SEND-MESS, JUST ADD THIS
      *             PARAGRAPH TO YOUR PROGRAM (SEE CL/LNVERI); MERC PR# 253
      *  JTG 030527 CORRECTED TO FORCE 0 ON NON RENEWALS SP-SCFRMLA 46,
      *                                                       LENDMARK  PR#2004
      *  BAH 050831 FORCE 0 ON NON RENEWALS SP-SCFRMLA 47, BAYC PR# 2582
      *  BAH 070227 ADDED VERIFY-MAKER-SUB TO KNOW WHEN YOU ARE TESTING
      *             A JOINT MAKER FOR A&H, DO NOT TEST AGE IF SP-INSAH-JOINT-FG
      *             IS NOT "Y". SUPPORT PL# 580
      *  BAH 110118 REMOVE "FB" FROM SCFRMLA 47 TEST, BAYC PR# 3714
      *  BAH 111114 REMOVE SCFRMLA 47 TEST, BAYC PR# 3898
      *  BAH 121009 ADDED SP-INSREJECT-FG "D" FOR MULLEN, PR# 4088
      *  BAH 130430 ADDED SP-INSREJECT-FG "E" FOR CASHCO, PR# 4261
      *  BAH 160630 ADDED TEST OF VERIFY-MAKER-SUB TO KNOW WHEN YOU ARE TESTING
      *             A JOINT MAKER, DO NOT TEST REJECT-TERM IF SP-INSAH-JOINT-FG
      *             IS NOT "Y". CITIZENS PL# 941
      *  BAH 170516 ADDED AMTYPE "R" TOTAL REPAY FOR REGMGM PR#5291
      *  BAH 2021.0813 ADDED SP-INSREJECT-FG "F" # 1510
      *  BAH 2023.0209 MOVE SPACES TO LIMIT-ERROR BEFORE WE CHECK THE EDT EDITS
      *                OR YOU GET A BOGUS PRINCIPAL BELOW MIN ERROR #CINDY
      *  BAH 2023.0302 ADDED SPTEST-INSCOVR-LIMITS, #1594
      *  BAH 2023.0419 CHANGED GP TO BR-TEST-PP-AUTO-INSCOVR #1594A
      *************************************************************************
       VERIFY-TERMCEIL SECTION.
           MOVE " " TO LIMIT-ERROR ST-FIRST-STEP MESS.
           MOVE "Y" TO VALID-LOAN.
           PERFORM LOAN-CALCULATIONS.

           IF VERIFY-REJECT-TERM-FG = "Y"
              PERFORM VERIFY-SET-REJECT-TERM
              GO TO VERIFY-TERMCEIL-EXIT.

           MOVE 0 TO SUB.
           PERFORM VERIFY-OT-INSURANCE.
           IF LN-INSOURS(3) = "Y"
              PERFORM VERIFY-PP-INSURANCE.

           PERFORM SPTEST-INS-VALID.
           IF BR-TEST-PP-AUTO-INSCOVR = "Y"
              PERFORM SPTEST-INSCOVR-LIMITS.
           PERFORM SPTEST-FIELD-VALID.
           PERFORM SPTEST-EXTDATE-VALID.

           MOVE SPACES TO LIMIT-ERROR.
           IF SP-CAL-NORATES(1) > 0
              SET CSTP TO SP-CAL-NORATES(1)
           ELSE
              SET CSTP TO 1.

           IF SP-CAL-SPFRMLA = 1
              SET ETBL TO 1
              GO TO VERIFY-TERMCEIL-NEXT.

           IF SP-CAL-BREAK-ON-TERM(1)
              IF SP-CAL-BREAK(1 CSTP) NOT ZERO
                AND LN-ORGTERM > SP-CAL-BREAK(1 CSTP)
                 SET ETBL TO 2
              ELSE
                 SET ETBL TO 1
           ELSE
              IF SP-CAL-BREAK(1 CSTP) NOT ZERO
                AND INT-CHARGEABLE > SP-CAL-BREAK(1 CSTP)
                 SET ETBL TO 2
              ELSE
                 SET ETBL TO 1.

       VERIFY-TERMCEIL-NEXT.
           ADD 1 TO SUB.
           IF SUB > SP-EDT-NOEDITS(ETBL)
              IF SP-CAL-SPFRMLA = 1 AND ETBL < 2
                 SET ETBL UP BY 1
                 MOVE 0 TO SUB
                 GO TO VERIFY-TERMCEIL-NEXT
              ELSE
                 GO TO VERIFY-TERMCEIL-EXIT.

           IF SP-EDT-TYPE(ETBL SUB) = "T" OR "R" OR "A"
              IF SP-EDT-COLL(ETBL SUB) NOT = " " AND
                 SP-EDT-COLL(ETBL SUB) NOT = SPTEST-CD-SECURED
                 GO TO VERIFY-TERMCEIL-NEXT.

           IF SP-EDT-TYPE(ETBL SUB) = "T"
              PERFORM VERIFY-TERMCEIL-TERM
           ELSE
              IF SP-EDT-TYPE(ETBL SUB) = "A"
                 PERFORM VERIFY-TERMCEIL-AMOUNT
              ELSE
                 IF SP-EDT-TYPE(ETBL SUB) = "R"
                    PERFORM VERIFY-TERMCEIL-RATE
                 ELSE
                    IF SP-EDT-TYPE(ETBL SUB) = "M" AND
                       ST-FIRST-STEP NOT = "N"
                       PERFORM VERIFY-STEP-TERM.

           IF LIMIT-ERROR = "Y"
              MOVE "N" TO VALID-LOAN
              MOVE " " TO LIMIT-ERROR
              PERFORM BOUNDARY-ERRMSG.
           GO TO VERIFY-TERMCEIL-NEXT.

       VERIFY-TERMCEIL-AMOUNT.
           PERFORM VERIFY-SET-N-WK1-AMT.
           IF SP-EDT-HL(ETBL SUB) = "L"
              IF N-WK1 < SP-EDT-AMT(ETBL SUB)
                 MOVE "Y" TO LIMIT-ERROR
              ELSE
                 NEXT SENTENCE
           ELSE
               IF SP-EDT-HL(ETBL SUB) = "H"
                  IF N-WK1 > SP-EDT-AMT(ETBL SUB)
                     MOVE "Y" TO LIMIT-ERROR.

       VERIFY-TERMCEIL-TERM.
           IF SP-EDT-TYPE(ETBL SUB) = "T"
              IF SP-EDT-HL(ETBL SUB) = "L"
                 IF LN-ORGTERM < SP-EDT-TERM(ETBL SUB)
                    MOVE "Y" TO LIMIT-ERROR
                 ELSE
                    NEXT SENTENCE
              ELSE
                  IF SP-EDT-HL(ETBL SUB) = "H"
                     IF LN-ORGTERM > SP-EDT-TERM(ETBL SUB)
                        MOVE "Y" TO LIMIT-ERROR.

       VERIFY-TERMCEIL-RATE.
           IF SP-EDT-TYPE(ETBL SUB) = "R"
              IF SP-EDT-HL(ETBL SUB) = "L"
                 IF SP-EDT-RATETYPE(ETBL SUB) = "A"
                    IF VERIFY-TERMCEIL-APR < SP-EDT-RATE(ETBL SUB)
                       MOVE "Y" TO LIMIT-ERROR
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF SP-EDT-RATETYPE(ETBL SUB) = "E" AND
                       VERIFY-TERMCEIL-EFF-FG = "Y"
                       IF VERIFY-TERMCEIL-EFF < SP-EDT-RATE(ETBL SUB)
                          MOVE "Y" TO LIMIT-ERROR.
           IF SP-EDT-TYPE(ETBL SUB) = "R"
              IF SP-EDT-HL(ETBL SUB) = "H"
                 IF SP-EDT-RATETYPE(ETBL SUB) = "A"
                    IF VERIFY-TERMCEIL-APR > SP-EDT-RATE(ETBL SUB)
                       MOVE "Y" TO LIMIT-ERROR
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF SP-EDT-RATETYPE(ETBL SUB) = "E" AND
                       VERIFY-TERMCEIL-EFF-FG = "Y"
                       IF VERIFY-TERMCEIL-EFF > SP-EDT-RATE(ETBL SUB)
                          MOVE "Y" TO LIMIT-ERROR.

       BOUNDARY-ERRMSG.
           MOVE 0 TO LIMIT-AMT.
           MOVE " " TO MESS LIMIT-TYPE-LIT LIMIT-AMT-TYPE
              LIMIT-AMT-LIT.

           IF SP-EDT-TYPE(ETBL SUB) = "A" OR "M"
              IF SP-EDT-AMTYPE(ETBL SUB) = "P"
                 MOVE "PRINCIPAL" TO LIMIT-AMT-TYPE
                 MOVE SP-EDT-AMT(ETBL SUB) TO LIMIT-AMT
              ELSE
                 IF SP-EDT-AMTYPE(ETBL SUB) = "N"
                    MOVE "TOTAL NOTE" TO LIMIT-AMT-TYPE
                    MOVE SP-EDT-AMT(ETBL SUB) TO LIMIT-AMT
                 ELSE
                    IF SP-EDT-AMTYPE(ETBL SUB) = "C"
                       MOVE "CASH ADVANCE" TO LIMIT-AMT-TYPE
                       MOVE SP-EDT-AMT(ETBL SUB) TO LIMIT-AMT
                    ELSE
                       IF SP-EDT-AMTYPE(ETBL SUB) = "X"
                          MOVE "SPL CASH ADV" TO LIMIT-AMT-TYPE
                          MOVE SP-EDT-AMT(ETBL SUB) TO LIMIT-AMT
                       ELSE
                          IF SP-EDT-AMTYPE(ETBL SUB) = "A"
                             MOVE "ADJ PRINCIPAL" TO LIMIT-AMT-TYPE
                             MOVE SP-EDT-AMT(ETBL SUB) TO LIMIT-AMT
                          ELSE
                          IF SP-EDT-AMTYPE(ETBL SUB) = "R"
                             MOVE "TOTAL REPAY" TO LIMIT-AMT-TYPE
                             MOVE SP-EDT-AMT(ETBL SUB) TO LIMIT-AMT.

           IF SP-EDT-TYPE(ETBL SUB) = "T"
              MOVE "TERM" TO LIMIT-AMT-TYPE
              MOVE SP-EDT-TERM(ETBL SUB) TO LIMIT-TERM.

           IF SP-EDT-TYPE(ETBL SUB) = "R"
              IF SP-EDT-RATETYPE(ETBL SUB) = "A"
                 MOVE "APR RATE" TO LIMIT-AMT-TYPE
                 MOVE SP-EDT-RATE(ETBL SUB) TO LIMIT-RATE
              ELSE
                 IF SP-EDT-RATETYPE(ETBL SUB) = "E"
                 MOVE "EFFECTIVE RATE" TO LIMIT-AMT-TYPE
                 MOVE SP-EDT-RATE(ETBL SUB) TO LIMIT-RATE.

           IF SP-EDT-HL(ETBL SUB) = "L"
              MOVE "BELOW MINIMUM OF" TO LIMIT-TYPE-LIT
           ELSE
              IF SP-EDT-HL(ETBL SUB) = "H"
                 MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT.

           IF SP-EDT-COLL(ETBL SUB) = "S"
              MOVE "- SEC. COLL." TO LIMIT-COLL-TYPE
           ELSE
              IF SP-EDT-COLL(ETBL SUB) = "U"
                 MOVE "- UNSEC. COLL." TO LIMIT-COLL-TYPE
              ELSE
                 IF SP-EDT-COLL(ETBL SUB) = "R"
                    MOVE "- R.E. SEC. COLL." TO LIMIT-COLL-TYPE
                 ELSE
                    MOVE " " TO LIMIT-COLL-TYPE.

           IF SP-EDT-TYPE(ETBL SUB) = "M"
              STRING "TERM OUTSIDE OF RANGE FOR " DELIMITED BY SIZE
                 LIMIT-AMT-TYPE DELIMITED BY "  " INTO MESS
              PERFORM CONDITIONAL-SEND-MESS
           ELSE
              UNSTRING LIMIT-AMT-LIT DELIMITED BY ALL " " INTO
                 LIMIT-WORKER LIMIT-AMT-LIT
              STRING LIMIT-AMT-TYPE, " ", LIMIT-TYPE-LIT, " ",
                 LIMIT-AMT-LIT, " ", LIMIT-COLL-TYPE
                 DELIMITED BY "  " INTO MESS
              PERFORM CONDITIONAL-SEND-MESS.

       VERIFY-TERMCEIL-EXIT.
           IF VALID-LOAN = "N"
              MOVE "CANNOT UPDATE - CORRECT ENTRIES TO CONTINUE"
                TO MESS
              PERFORM CONDITIONAL-SEND-MESS.

      *************************************
       VERIFY-STEP-TERM SECTION.
      *************************************
           PERFORM VERIFY-SET-N-WK1-AMT.
           MOVE "N"         TO ST-FIRST-STEP.
           MOVE 999         TO ST-MAXTERM.
           MOVE 99999999.99 TO ST-MAXAMT.
           MOVE 0           TO ST-MINTERM ST-MINAMT.
           MOVE ETBL        TO STSUB-1.
           SUBTRACT 1 FROM SUB GIVING STSUB-2.

       VERIFY-ST-MINMAX.
           ADD 1 TO STSUB-2.
           IF STSUB-2 > SP-EDT-NOEDITS(STSUB-1)
              IF STSUB-1 < 2
                 IF SP-CAL-SPFRMLA = 1
                    ADD 1 TO STSUB-1
                    MOVE 0 TO STSUB-2
                    GO TO VERIFY-ST-MINMAX
                 ELSE
                    GO TO VERIFY-ST-RANGE
              ELSE
                 GO TO VERIFY-ST-RANGE.
           IF SP-EDT-AMT(STSUB-1 STSUB-2) NOT < N-WK1 AND
              SP-EDT-AMT(STSUB-1 STSUB-2) < ST-MAXAMT
              MOVE SP-EDT-AMT(STSUB-1 STSUB-2) TO ST-MAXAMT
              MOVE SP-EDT-TERM(STSUB-1 STSUB-2) TO ST-MAXTERM.
           GO TO VERIFY-ST-MINMAX.

       VERIFY-ST-RANGE.
           IF LN-ORGTERM NOT > ST-MAXTERM
              GO TO VERIFY-STEP-TERM-EXIT
           ELSE
              MOVE "Y" TO LIMIT-ERROR.
       VERIFY-STEP-TERM-EXIT.
           EXIT.

      *************************************
       SPTEST-FIELD-VALID SECTION.
      *************************************
           MOVE " " TO LIMIT-AMT-LIT.
           IF SP-SCFRMLA NOT = 0
      * LENDMARK - NEW SCFRMLA 46 EXACTLY LIKE 20 EXCEPT ONLY ON ACCOUNTS
      *            WITH SOURCE CODE OF THESE, NO RESTRICTIONS
              IF SP-SCFRMLA = 46
                 IF NOT (LN-SRCD = "PB" OR "RB" OR "RN" OR "SC")
                    GO TO SPTEST-MAINTFEE-VALID
                 END-IF
              END-IF
              IF LN-SERCHG < SP-SCMIN
                 MOVE "Y" TO LIMIT-ERROR
                 MOVE "BELOW MINIMUM OF" TO LIMIT-TYPE-LIT
                 MOVE SP-SCMIN TO LIMIT-AMT
              ELSE
                 IF LN-SERCHG > SP-SCMAX
                    MOVE "Y" TO LIMIT-ERROR
                    MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                    MOVE SP-SCMAX TO LIMIT-AMT.
           IF LIMIT-ERROR = "Y"
              MOVE "N" TO VALID-LOAN
              MOVE " " TO LIMIT-ERROR
              MOVE "SERVICE CHARGE" TO LIMIT-AMT-TYPE
              PERFORM DSPLY-BOUNDARY-ERRMSG.

       SPTEST-MAINTFEE-VALID.
           MOVE " " TO LIMIT-AMT-LIT.
           IF SP-MFFRMLA NOT = 0
              MULTIPLY LN-MAINTFEE BY LN-ORGTERM GIVING
                 SPTEST-MFYEARLY ROUNDED
              IF SPTEST-MFYEARLY  < SP-MFMIN
                 MOVE "Y" TO LIMIT-ERROR
                 MOVE "BELOW MINIMUM OF" TO LIMIT-TYPE-LIT
                 MOVE SP-MFMIN TO LIMIT-AMT
              ELSE
                 IF SPTEST-MFYEARLY > SP-MFMAX
                    MOVE "Y" TO LIMIT-ERROR
                    MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                    MOVE SP-MFMAX TO LIMIT-AMT.
           IF LIMIT-ERROR = "Y"
              MOVE "N" TO VALID-LOAN
              MOVE " " TO LIMIT-ERROR
              MOVE "MAINT. FEE" TO LIMIT-AMT-TYPE
              PERFORM DSPLY-BOUNDARY-ERRMSG.

       SPTEST-FIELD-VALID-EXIT.
           EXIT.

      ******************************************
       SPTEST-INS-VALID SECTION.
      ******************************************
           IF LN-INSTYPES-DL
              MOVE "DL-" TO SPTEST-INSTYPE
              MOVE 1 TO SPT1
              IF LN-INSURED = "J"
                 MOVE 2 TO SPT2
                 PERFORM SPTEST-INS-AMTS
              ELSE
                 MOVE 1 TO SPT2
                 PERFORM SPTEST-INS-AMTS.
           IF LN-INSTYPES-LL
              MOVE 1 TO SPT1
              MOVE "LL-" TO SPTEST-INSTYPE
              IF LN-INSURED = "J"
                 MOVE 4 TO SPT2
                 PERFORM SPTEST-INS-AMTS
              ELSE
                 MOVE 3 TO SPT2
                 PERFORM SPTEST-INS-AMTS.
           IF LN-INSTYPES-AH
              MOVE 2 TO SPT1
              MOVE 5 TO SPT2
              MOVE "AH-" TO SPTEST-INSTYPE
      * DONT TEST JOINT MAKERS AGE RESTRICTIONS IF SPR DOES NOT ALLOW JOINT
      * YOU HAD TO KNOW IF THIS WAS TESTING THE JOINT, THATS WHAT
      * VERIFY-MAKER-SUB IS, 1 = SSNO(1), 2 = SSNO(2), ETC
              IF (LN-INSURED = "J") AND (VERIFY-MAKER-SUB NOT = 1)
                    AND (SP-INSAH-JOINT-FG NOT = "Y")
                       MOVE 0 TO VERIFY-AGE
              END-IF
              PERFORM SPTEST-INS-AMTS.
           IF LN-INSTYPES-PP OR LN-INSTYPES-PP2
              MOVE 3 TO SPT1
              MOVE "PP-" TO SPTEST-INSTYPE
              IF LN-INSTYPES-PP
                 MOVE 6 TO SPT2
                 PERFORM SPTEST-INS-AMTS
              ELSE
                 MOVE 7 TO SPT2
                 PERFORM SPTEST-INS-AMTS.
           IF LN-INSTYPES-O1
              MOVE 4 TO SPT1
              MOVE 8 TO SPT2
              MOVE "O1-" TO SPTEST-INSTYPE
              PERFORM SPTEST-INS-AMTS.

      ***************************************************
       SPTEST-INSCOVR-LIMITS SECTION.
      ***************************************************

      * WORLD NEEDS TO TEST PP + VSI COVERAGE NOT GREATER THAN MAX CHARGEABLE
      * IF THEY HAVE BOTH INSURANCES, USER IS SUPPOSED TO REDUCE ONE OF THE
      * COVERAGES
 
      * PAGE 17 SP-INS-O1-EDIT "I" OTHER 1 = IUI
      *                        "C" OTHER 1 = AUTO OR CPI
           IF SP-INS-O1-EDIT = "I"
              COMPUTE SPTEST-INSCOVR-TEST = LN-INSCOVR(3) +
                                            LN-INSCOVR(6)
           ELSE
              COMPUTE SPTEST-INSCOVR-TEST = LN-INSCOVR(3) +
                                            LN-INSCOVR(4).
 
      * THEY DIDNT WRITE ANY OF THE INSURANCE
           IF SPTEST-INSCOVR-TEST = 0
              GO TO EXIT-SPTEST-INSCOVR-LIMIT-CONTROL.

      * KEVIN SAID TESTING PP1 IS ALL THAT IS NEEDED, SUB (6)
      * P  SP-INS-PRINCIPAL
      * N  SP-INS-TOTAL-NOTE
      * R  SP-INS-ALL
      * M  SP-INS-TOTAL-NOTE-EXT

           IF SP-INS-TOTAL-NOTE(6)
              MOVE TOTAL-NOTE TO SPTEST-MAX-INS-CHARGEABLE
           ELSE
           IF SP-INS-TOTAL-NOTE-EXT(6)
              ADD TOTAL-NOTE LN-EXTCHG GIVING SPTEST-MAX-INS-CHARGEABLE
           ELSE
           IF SP-INS-ALL(6)
              MOVE TOTAL-REPAY TO SPTEST-MAX-INS-CHARGEABLE
           ELSE
           IF SP-INS-PRINCIPAL(6)
              MOVE DISCLOSED-FINAMT TO SPTEST-MAX-INS-CHARGEABLE.

      * STATES "TN", "OK" AND "MS" USE OTHER 1 FOR AUTO
      * ALL OTHER STATES, USE OTHER 3
      * IF SP-INS-O1-EDIT IS "I", IDENTIFIES OTHER 1 AS IUI, SO USE OTHER 3

           IF SPTEST-INSCOVR-TEST > SPTEST-MAX-INS-CHARGEABLE
              MOVE SPTEST-MAX-INS-CHARGEABLE TO SPTEST-MAX-INS-DISPLAY
              MOVE SPACES TO MESS
              IF SP-INS-O1-EDIT = "I"
                 STRING "PP COVR + O3 COVR EXCEEDS LIMIT OF "
                         SPTEST-MAX-INS-DISPLAY
                         DELIMITED BY "  " INTO MESS
              ELSE
                 STRING "PP COVR + O1 COVR EXCEEDS LIMIT OF "
                         SPTEST-MAX-INS-DISPLAY
                         DELIMITED BY "  " INTO MESS
              END-IF
              PERFORM SEND-MESS
              MOVE "N" TO VALID-LOAN
              GO TO EXIT-SPTEST-INSCOVR-LIMIT-CONTROL.

       EXIT-SPTEST-INSCOVR-LIMIT-CONTROL.
           EXIT.

      ***********************************************
       SPTEST-INS-AMTS SECTION.
      ***********************************************
      * MULLEN PR# 4088 CREDIT DISABILITY INSURANCE REJECT CODE IF
      *                 BORROWER WORK HOURS ARE LESS THAN 30. THEY ARE
      *                 ENTERING A NUMBER OF HOURS ONLY IN BW-WORKHRS
           IF LN-INSURED NOT = "N"
              IF SP-INSREJECT-FG(SPT2) = "D"
                 MOVE BW-WORKHRS TO WORK-HOURS-ALPHA
                 CALL "C$JUSTIFY" USING WORK-HOURS-ALPHA, "R"
                 INSPECT WORK-HOURS-ALPHA REPLACING ALL " " BY "0"
                 IF WORK-HOURS-NUM < 30
                    MOVE "WORK HRS BELOW MINIMUM OF " TO LIMIT-TYPE-LIT
                    MOVE 30 TO LIMIT-TERM
                    MOVE "N" TO VALID-LOAN
                    PERFORM DSPLY-BOUNDARY-ERRMSG.

      * WORLD 8/13/2021 COMPUTE NEW BORROWER AGE SUBTRACTING INSURANCE TERM FROM
      *                 BIRTHDATE. FORTEGRA WANTS "ATTAINED AGE AT EXPIRATION"
      * SEE SPR HELP FOR AN EXAMPLE
           IF SP-INSREJECT-FG(SPT2) = "F"
              PERFORM SPTEST-COMPUTE-VERIFY-AGE
              MOVE NDTE-DATE TO VERIFY-AGE.

           IF VERIFY-AGE = 0 OR LN-INSURED = "N"
              MOVE 0 TO BAGE-AGE
           ELSE
              MOVE VERIFY-AGE        TO BAGE-DATE
              MOVE VERIFY-REFDATE    TO BAGE-REFDATE
              MOVE VERIFY-TODAY-DATE TO BAGE-TODAY
              IF SP-INSREJECT-FG(SPT2) = "A"
                 MOVE BAGE-TODAY TO NUM-DATE
                 MOVE 12 TO NUM-MO
                 MOVE 31 TO NUM-DA
                 MOVE NUM-DATE   TO BAGE-TODAY
              END-IF
              PERFORM BAGE-CALCULATION.

           IF LN-INSOURS(SPT1) = "Y"
              IF LN-INSPREM(SPT1) < SP-INSMINPREM(SPT2)
                 MOVE "BELOW MINIMUM OF" TO LIMIT-TYPE-LIT
                 MOVE " " TO LIMIT-AMT-TYPE
                 STRING SPTEST-INSTYPE "PREMIUM" DELIMITED BY "  "
                   INTO LIMIT-AMT-TYPE
                 MOVE SP-INSMINPREM(SPT2) TO LIMIT-AMT
                 MOVE "N" TO VALID-LOAN
                 PERFORM DSPLY-BOUNDARY-ERRMSG.

           IF LN-INSOURS(SPT1) = "Y"
             IF LN-INSCOVR(SPT1) < SP-INSMINCHGABL(SPT2)
                MOVE "BELOW MINIMUM OF" TO LIMIT-TYPE-LIT
                MOVE " " TO LIMIT-AMT-TYPE
                STRING SPTEST-INSTYPE "CHARGEABLE" DELIMITED BY "  "
                   INTO LIMIT-AMT-TYPE
                MOVE SP-INSMINCHGABL(SPT2) TO LIMIT-AMT
                MOVE "N" TO VALID-LOAN
                PERFORM DSPLY-BOUNDARY-ERRMSG
            ELSE
              IF NOT SP-INSMAXCHGABL-OVRIDE(SPT2)
               IF LN-INSCOVR(SPT1) > SP-INSMAXCHGABL(SPT2)
                 MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                 MOVE " " TO LIMIT-AMT-TYPE
                 STRING SPTEST-INSTYPE "CHARGEABLE" DELIMITED BY "  "
                    INTO LIMIT-AMT-TYPE
                 MOVE SP-INSMAXCHGABL(SPT2) TO LIMIT-AMT
                 MOVE "N" TO VALID-LOAN
                 PERFORM DSPLY-BOUNDARY-ERRMSG.

           IF LN-INSOURS(SPT1) = "Y"
              IF LN-INSTRM(SPT1) > SP-INSMAXTERM(SPT2)
                 MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                 MOVE " " TO LIMIT-AMT-TYPE
                 STRING SPTEST-INSTYPE "TERM" DELIMITED BY "  "
                    INTO LIMIT-AMT-TYPE
                 MOVE SP-INSMAXTERM(SPT2) TO LIMIT-TERM
                 MOVE "N" TO VALID-LOAN
                 PERFORM DSPLY-BOUNDARY-ERRMSG.

           IF ( BAGE-AGE = 0               ) OR
              ( SP-INSMAXAGE (SPT2,1) = 0  ) OR
              ( LN-INSOURS(SPT1) NOT = "Y" )
              GO TO SPTEST-INS-AMTS-EXIT.

      * BEGIN SEARCH FOR AGE:
           MOVE BAGE-AGE TO BAGE-TEST.
           IF SP-INSREJECT-FG(SPT2) = "A"
              COMPUTE BAGE-TEST = BAGE-AGE + (LN-ORGTERM/12)
           ELSE
           IF SP-INSREJECT-FG(SPT2) = "B"
              COMPUTE BAGE-TEST = BAGE-AGE + (LN-INSTRM(SPT1)/12)
              DIVIDE LN-INSTRM(SPT1) BY 12 GIVING BAGE-TODAY
                 REMAINDER BAGE-TODAY
              IF BAGE-TODAY NOT = 0
                 ADD 1 TO BAGE-TEST.

           IF SP-INSREJECT-FG(SPT2) NOT = "C"
              SET IVER TO 1
              SEARCH SP-INSVERTBL
                END  MOVE " " TO MESS
                     STRING SPTEST-INSTYPE
                            "COVERAGE PROHIBITED RE: BORROWERS AGE"
                            DELIMITED BY "  " INTO MESS
                     PERFORM CONDITIONAL-SEND-MESS
                     MOVE "N" TO VALID-LOAN
                     GO TO SPTEST-INS-AMTS-EXIT
                WHEN (
                       BAGE-TEST >= SP-INSMINAGE(SPT2,IVER) AND
                       BAGE-TEST <= SP-INSMAXAGE(SPT2,IVER)
                     )
              END-SEARCH
           ELSE
              MOVE 1 TO IVER
              IF BAGE-TEST < SP-INSMINAGE(SPT2,IVER) OR
                 BAGE-TEST > SP-INSMAXAGE(SPT2,IVER)
                 MOVE " " TO MESS
                 STRING SPTEST-INSTYPE
                    "COVERAGE PROHIBITED RE: BORROWERS AGE"
                     DELIMITED BY "  " INTO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "N" TO VALID-LOAN
                 GO TO SPTEST-INS-AMTS-EXIT.

           IF LN-INSOURS(SPT1) = "Y"
              IF SP-INSREJECT-FG(SPT2) = "E"
                 IF LN-INSTRM(SPT1) >
                  ((SP-INSMAXAGE(SPT2,IVER) * 12) - BAGE-AGE-MONTHS)
                    MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                    MOVE " " TO LIMIT-AMT-TYPE
                    STRING SPTEST-INSTYPE
                          "TERM FOR BORR AGE" DELIMITED BY "  "
                                            INTO LIMIT-AMT-TYPE
                    COMPUTE LIMIT-TERM =
                  ((SP-INSMAXAGE(SPT2,IVER) * 12) - BAGE-AGE-MONTHS)
                    MOVE "N" TO VALID-LOAN
                    PERFORM DSPLY-BOUNDARY-ERRMSG.

           IF LN-INSOURS(SPT1) = "Y"
              IF LN-INSTRM(SPT1) > SP-INSMAXTRM(SPT2,IVER)
                 MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                 MOVE " " TO LIMIT-AMT-TYPE
                 STRING SPTEST-INSTYPE
                       "TERM FOR BORR AGE" DELIMITED BY "  "
                          INTO LIMIT-AMT-TYPE
                 MOVE SP-INSMAXTRM(SPT2,IVER) TO LIMIT-TERM
                 MOVE "N" TO VALID-LOAN
                 PERFORM DSPLY-BOUNDARY-ERRMSG.

           IF LN-INSOURS(SPT1) = "Y"
              IF SP-INSCHGBEN(SPT2) = "B"
                 SUBTRACT LN-MAINTFEE FROM LN-REGPYAMT GIVING
                                             SPTEST-BENEFIT
                 IF SPTEST-BENEFIT > SP-INSMAXCOV(SPT2,IVER)
                    MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                    MOVE " " TO LIMIT-AMT-TYPE
                    STRING SPTEST-INSTYPE
                         "BENEFIT FOR BORR AGE" DELIMITED BY "  "
                            INTO LIMIT-AMT-TYPE
                    MOVE SP-INSMAXCOV(SPT2,IVER) TO LIMIT-AMT
                    MOVE "N" TO VALID-LOAN
                    PERFORM DSPLY-BOUNDARY-ERRMSG.

           IF LN-INSOURS(SPT1) = "Y"
              IF SP-INSCHGBEN(SPT2) = "C"
                 IF LN-INSCOVR(SPT1) > SP-INSMAXCOV(SPT2,IVER)
                    MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                    MOVE " " TO LIMIT-AMT-TYPE
                    STRING SPTEST-INSTYPE
                         "COVERAGE FOR BORR AGE" DELIMITED BY "  "
                            INTO LIMIT-AMT-TYPE
                    MOVE SP-INSMAXCOV(SPT2,IVER) TO LIMIT-AMT
                    MOVE "N" TO VALID-LOAN
                    PERFORM DSPLY-BOUNDARY-ERRMSG.

           GO TO SPTEST-INS-AMTS-EXIT.

      *SPTEST-INS-AMTS-NOT-FOUND.
      *    MOVE " " TO MESS.
      *    STRING SPTEST-INSTYPE
      *         "COVERAGE PROHIBITED RE: BORROWERS AGE"
      *              DELIMITED BY "  " INTO MESS.
      *    PERFORM CONDITIONAL-SEND-MESS.
      *    MOVE "N" TO VALID-LOAN.

       SPTEST-INS-AMTS-EXIT.
           EXIT.

      * XONPC0 CALLS THIS ROUTINE TWICE AND IT THEN TRIED TO SUBTRACT AGAIN
      * ASSUMING THE BW-BDATE IS FOR THE CORRECT PERSON, #1510
      *********************************************************
       SPTEST-COMPUTE-VERIFY-AGE SECTION.
      *********************************************************
           MOVE BW-BDATE   TO VERIFY-AGE BAGE-DATE NDTE-DATE.
           COMPUTE NDTE-HOLD = LN-INSTRM(SPT1) * -1.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO VERIFY-AGE.


       DSPLY-BOUNDARY-ERRMSG SECTION.
           UNSTRING LIMIT-AMT-LIT DELIMITED BY ALL " " INTO
              LIMIT-WORKER LIMIT-AMT-LIT.
           STRING LIMIT-AMT-TYPE, " ", LIMIT-TYPE-LIT, " "
              LIMIT-AMT-LIT, " ", "- CANNOT UPDATE."
              DELIMITED BY "  " INTO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
           MOVE " " TO LIMIT-AMT-LIT MESS.


       SPTEST-EXTDATE-VALID SECTION.
           MOVE " " TO LIMIT-ERROR.
           MOVE SP-PYEXYEARTP TO ELAPSED-YRTYPE.
           MOVE LN-1STPYDATE TO SYS-DATE.
           IF SP-PYEXFMDATE-LOAN-DT
              MOVE LN-LOANDATE TO NUM-DATE
           ELSE
              MOVE LN-INTDATE TO NUM-DATE.
           PERFORM TIMALL.

           IF SP-PYEXBASECD-DAYS
              ADD SP-PYEXBASE SP-PYEXDAYS
                   GIVING AMT-HOLDER
              IF ELAPSED-DAYS > AMT-HOLDER
                 GO TO SPTEST-EXTDATE-EXCEEDS-MAX.
           IF SP-PYEXBASECD-MTHS
              IF ELAPSED-MONTHS > SP-PYEXBASE
                 GO TO SPTEST-EXTDATE-EXCEEDS-MAX
              ELSE
                 IF ELAPSED-MONTHS = SP-PYEXBASE AND
                    ELAPSED-REM > SP-PYEXDAYS
                    GO TO SPTEST-EXTDATE-EXCEEDS-MAX.
           GO TO SPTEST-EXTDATE-VALID-EXIT.

       SPTEST-EXTDATE-EXCEEDS-MAX.
           MOVE "EXTENSION PERIOD EXCEEDS TIME ALLOWED" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
           MOVE "N" TO VALID-LOAN.
           MOVE "Y" TO LIMIT-ERROR.

       SPTEST-EXTDATE-VALID-EXIT.
           EXIT.

       VERIFY-SET-N-WK1-AMT SECTION.
           IF SP-EDT-AMTYPE(ETBL SUB) = "P"
              MOVE DISCLOSED-FINAMT TO N-WK1
           ELSE
              IF SP-EDT-AMTYPE(ETBL SUB) = "N"
                 MOVE TOTAL-NOTE TO N-WK1
              ELSE
                 IF SP-EDT-AMTYPE(ETBL SUB) = "C"
                    MOVE CASH-ADVANCE TO N-WK1
                 ELSE
                    IF SP-EDT-AMTYPE(ETBL SUB) = "X"
                       MOVE SPCASH-ADVANCE TO N-WK1
                    ELSE
                       IF SP-EDT-AMTYPE(ETBL SUB) = "A"
                          MOVE ADJ-PRINCIPAL TO N-WK1
                       ELSE
                          IF SP-EDT-AMTYPE(ETBL SUB) = "R"
                             MOVE TOTAL-REPAY TO N-WK1
                          ELSE
                             MOVE 0 TO N-WK1.

       VERIFY-OT-INSURANCE SECTION.
           COMPUTE N-WK1 ROUNDED = FINANCED-AMOUNT / 3.

           IF SP-INS-O1-EDIT = "A"
              IF LN-INSOURS(4) = "Y"
                 IF LN-INSPREM(4) > N-WK1
                    MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                    MOVE " " TO LIMIT-AMT-TYPE
                    STRING "O1-INS.PREMIUM" DELIMITED BY "  "
                       INTO LIMIT-AMT-TYPE
                    MOVE N-WK1 TO LIMIT-AMT
                   MOVE "N" TO VALID-LOAN
                    PERFORM DSPLY-BOUNDARY-ERRMSG.
           IF SP-INS-O2-EDIT = "A"
              IF LN-INSOURS(5) = "Y"
                 IF LN-INSPREM(5) > N-WK1
                    MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                    MOVE " " TO LIMIT-AMT-TYPE
                    STRING "O2-INS.PREMIUM" DELIMITED BY "  "
                       INTO LIMIT-AMT-TYPE
                    MOVE N-WK1 TO LIMIT-AMT
                    MOVE "N" TO VALID-LOAN
                    PERFORM DSPLY-BOUNDARY-ERRMSG.

       VERIFY-OT-INSURANCE-EXIT.
           EXIT.

       VERIFY-PP-INSURANCE SECTION.

           IF LN-INSTYPES-PP
              IF SP-WDT-DUAL-EDIT(6)
                 IF TOTAL-REPAY < 500
                    MOVE "LOAN AMOUNT UNDER DUAL PROPERTY AMT"
                     TO LIMIT-AMT-LIT
                 MOVE "N" TO VALID-LOAN
                 PERFORM DSPLY-BOUNDARY-ERRMSG.
           IF LN-INSTYPES-PP2
              IF SP-WDT-DUAL-EDIT(7)
                 IF TOTAL-REPAY < 500
                    MOVE "LOAN AMOUNT UNDER DUAL PROPERTY AMT"
                     TO LIMIT-AMT-LIT
                 MOVE "N" TO VALID-LOAN
                 PERFORM DSPLY-BOUNDARY-ERRMSG.

      ***************************************************
      *    VERIFY-SET-REJECT-TERM
      ***************************************************
       VERIFY-SET-REJECT-TERM SECTION.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              MOVE 0 TO VERIFY-REJECT-TERM(SUB)
           END-PERFORM.

           IF VERIFY-AGE = 0 OR LN-INSURED = "N"
              MOVE 0 TO BAGE-AGE
              GO TO VERIFY-SET-REJECT-TERM-EXIT.

      * ESTABLISH AGE OF BORROWER AT LOAN MATURITY:
           MOVE VERIFY-AGE        TO BAGE-DATE.
           MOVE VERIFY-REFDATE    TO BAGE-REFDATE.
           MOVE VERIFY-TODAY-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

           IF LN-INSTYPES-DL
              MOVE "DL-" TO SPTEST-INSTYPE
              MOVE 1 TO SPT1
              IF LN-INSURED = "J"
                 MOVE 2 TO SPT2
                 PERFORM VERIFY-SET-REJECT-TERM-OK
              ELSE
                 MOVE 1 TO SPT2
                 PERFORM VERIFY-SET-REJECT-TERM-OK.
           IF LN-INSTYPES-LL
              MOVE "LL-" TO SPTEST-INSTYPE
              MOVE 1 TO SPT1
              IF LN-INSURED = "J"
                 MOVE 4 TO SPT2
                 PERFORM VERIFY-SET-REJECT-TERM-OK
              ELSE
                 MOVE 3 TO SPT2
                 PERFORM VERIFY-SET-REJECT-TERM-OK.
           IF LN-INSTYPES-AH
              MOVE "AH-" TO SPTEST-INSTYPE
              MOVE 2 TO SPT1
              MOVE 5 TO SPT2
      * DONT TEST JOINT MAKERS AGE RESTRICTIONS IF SPR DOES NOT ALLOW JOINT
      * YOU HAD TO KNOW IF THIS WAS TESTING THE JOINT, THATS WHAT
      * VERIFY-MAKER-SUB IS, 1 = SSNO(1), 2 = SSNO(2), ETC
              IF (LN-INSURED = "J") AND (VERIFY-MAKER-SUB NOT = 1)
                    AND (SP-INSAH-JOINT-FG NOT = "Y")
                       MOVE 0 TO VERIFY-AGE BAGE-AGE
                       GO TO VERIFY-SET-REJECT-TERM-EXIT
              ELSE
                  PERFORM VERIFY-SET-REJECT-TERM-OK.
           IF LN-INSTYPES-PP OR LN-INSTYPES-PP2
              MOVE 3 TO SPT1
              MOVE "PP-" TO SPTEST-INSTYPE
              IF LN-INSTYPES-PP
                 MOVE 6 TO SPT2
                 PERFORM VERIFY-SET-REJECT-TERM-OK
              ELSE
                 MOVE 7 TO SPT2
                 PERFORM VERIFY-SET-REJECT-TERM-OK.
           IF LN-INSTYPES-O1
              MOVE "O1-" TO SPTEST-INSTYPE
              MOVE 4 TO SPT1
              MOVE 8 TO SPT2
              PERFORM VERIFY-SET-REJECT-TERM-OK.

       VERIFY-SET-REJECT-TERM-EXIT.
           EXIT.

      ***************************************************
      *    VERIFY-SET-REJECT-TERM-OK
      ***************************************************
       VERIFY-SET-REJECT-TERM-OK SECTION.

           IF SP-INSREJECT-FG(SPT2) NOT = "C"
              MOVE 0 TO VERIFY-REJECT-TERM(SPT1)
              GO TO VERIFY-SET-REJECT-TERM-OK-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SET OF REJECT-TERM FOR AGE, BASED ON BORROWER'S
      * AGE ON MATURITY OF LOAN:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BAGE-AGE TO BAGE-TEST.
           MOVE 1 TO IVER.
           IF BAGE-TEST < SP-INSMINAGE(SPT2,IVER)
              MOVE 0 TO VERIFY-REJECT-TERM(SPT1)
              GO TO VERIFY-SET-REJECT-TERM-OK-EXIT.

           IF BAGE-TEST <= SP-INSMAXAGE(SPT2,IVER)
              MOVE LN-ORGTERM TO VERIFY-REJECT-TERM(SPT1)
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *      BIRTH DATE OF BORROWER IS NOT AVAILABLE, SO SET INSURANCE
      *      TERM BY TAKING LOAN TERM BACK BY # OF YEARS BORROWER'S AGE
      *      IS OVER MAX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
             IF VERIFY-AGE <= 9999
                COMPUTE N-WK1 =
                     (BAGE-TEST - SP-INSMAXAGE(SPT2,IVER)) *12
                IF N-WK1 > LN-ORGTERM
                   MOVE 0 TO VERIFY-REJECT-TERM(SPT1)
                   GO TO VERIFY-SET-REJECT-TERM-OK-EXIT
                ELSE
                   SUBTRACT N-WK1 FROM LN-ORGTERM
                                 GIVING VERIFY-REJECT-TERM(SPT1)
                END-IF
             ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *      GET DATE ONE MONTH PRIOR TO BIRTHDATE AT INSURANCE MAXIMUM AGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                MOVE VERIFY-AGE TO NDTE-DATE
                IF NDTE-YY < EXT-JULIAN-CC
                   COMPUTE N-WK1 = (EXT-JULIAN-CC - NDTE-YY) * 12
                   MOVE EXT-JULIAN-CC TO NDTE-YY
                ELSE
                   MOVE 0 TO N-WK1
                END-IF
                COMPUTE NDTE-HOLD =
                   (SP-INSMAXAGE(SPT2,IVER) + 1) * 12 - N-WK1
                PERFORM INCREMENT-MONTHS
                MOVE NDTE-DATE         TO NUM-DATE
                MOVE VERIFY-TODAY-DATE TO SYS-DATE
                PERFORM TIM360
                SUBTRACT 1 ELAPSED-MONTHS FROM LN-ORGTERM
                             GIVING VERIFY-REJECT-TERM(SPT1).

           IF VERIFY-REJECT-TERM(SPT1) <= 0
              MOVE 9999 TO VERIFY-REJECT-TERM(SPT1)
              MOVE " " TO MESS
              STRING SPTEST-INSTYPE
                     "COVERAGE PROHIBITED RE: BORROWERS AGE"
                            DELIMITED BY "  " INTO MESS
              PERFORM CONDITIONAL-SEND-MESS.

       VERIFY-SET-REJECT-TERM-OK-EXIT.
           EXIT.

