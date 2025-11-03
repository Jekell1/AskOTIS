      * COPYMEMBER: LIBLP/LPAERN

      ******************************************************************
      *         COMPUTE MONTHLY ACCRUAL ANTICIPATED EARNINGS
      *VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      *   NEEDS WORK FOR BALLOON & UNIT PERIODS
      *^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      *
      *   NAME: LPAERN
      *   DESC: THIS ROUTINE COMPUTES EARNINGS FOR
      *         A GIVEN MONTH (ACCRUAL METHOD), BASED ON
      *         EARNINGS ALREADY TAKEN.
      *
      *   NEW   OLD
      *   ---  ----
      *    51  (15) -  RULE 78    ALL ANTICIPATED LESS UP-FRONT-EARNINGS
      *                OR PRORATA ACCRUAL INTEREST EARNINGS  VIA:AERN-64-VIA60
      *                   WHEN AERN = 2 AND AERN-ADDON-FG = " "     2542
      *    52  (12) -  ACTUARIAL  ALL ANTICIPATED
      *    55              "      (OLD VERSION OF ACTUARIAL)
      *    53  (14) -  PRORATA    ALL ANTICIPATED
      *    54  (11) -  RULE 78-5  ALL ANTICIPATED LESS UP-FRONT-EARNINGS
      *    56       -  RULE 78    SAME AS CASH EARNINGS METHOD 7
      *    57       -  ACTUARIAL  ACTUARIAL PRINICPAL REDUCTION, BASED ON A
      *                           THE DIFFERENCE BETWEEN EARNINGS FOR THE
      *                           YIELD (INTEREST AND DEALERL DISCOUNT)
      *                           AND EARNINGS FOR INTEREST
      *    58  (58) -  RULE 78    CASH LIQUIDATION IF PAID AHEAD OR
      *                           TIME (ACCRUAL) IF DELIQUENCY
      *    59  (59) -  REBATE     TRUE REBATE METHOD
      *    60       -  ACTUARIAL    (MERCURY)
      *    61       -  OPEN
      *    62       -  ACTUARIAL  PRORATION OF COMPONENT TO THE TOTAL
      *                           YIELD FOR INTEREST, SERVICE CHARGE,
      *                           MAINTENANCE FEES AND DEALER DISCOUNT
      *                           (AUTOPRIME)
      *    63       -  ACTUARIAL  FOR INTEREST AND/OR DISCOUNT (AUTOPRIME)
      *                  INTEREST - COMPUTE EARNINGS ACTUARIAL BASED ON APR
      *                  DISCOUNT - COMPUTE EARNINGS ACTUARIAL USING (YIELD)
      *                             OF INTEREST AND DEALER DISCOUNT, THAN
      *                             REDUCE YIELD EARNINGS BY INTEREST EARNINGS
      *                             PER ABOVE
      *    64       -  SAME RATIO OF EARNINGS AS INTEREST (MERCURY..DISCOUNT)
      *                NOTE:  WHEN ACCOUNT IS 'IB' ACCOUNT IS VIEWED AS IF IT
      *                       WAS 'PC' AND EARNINGS FOR PERIOD IS DETERMINED
      *                       VIA ERNFRMLA = 60
      *
      *                       ...WHEN THE INTEREST CHARGE IS 0, EARNINGS WILL
      *                          BE COMPUTED VIA SP-ERNFRMLA 51 (RULE 78THS)
      *    65       -  USE SP-ERNFRMLA 52 CALCULATIONS, HOWEVER USE CASH
      *                (PAYMENTS TO DATE + EXCEPTION PAYMENTS) TO DETERMINE
      *                AERN-ENDDATE (INSTEAD OF M/E DATE) TO CALCULATE THE
      *                ACTUARIAL REBATE.  MUST BE GP-I-AM-WORLD TO USE.
      *                THIS METHOD SHOULD HELP WORLD DETERMINE THE DIFFERENCES
      *                BETWEEN 52 ACCRUAL & 52 CASH  PR#930
      *
      *   IN  : AERN-ENDDATE               ACCRUAL ENDING DATE
      *         AERN-LAST-ACCRUAL-DATE     LN-LAST-ACCRUAL-DATE
      *         AERN
      *             0          - ALL COMPONENTS (DEFAULT)
      *             1 TO 6     - THAT COMPONENT
      *
      * ADDON  N O T E:
      *  WHEN EARNINGS ARE CALCULATED FOR ADDON INSURANCE,
      *  AERN-ADDON-FG MUST BE SET = "Y" AND THE FOLLOWING FIELDS
      *  ARE INITIALIZED WITH THE CORRECT VALUES:
      *
      *      AERN-LN-ORIG-1STPYDATE AERN-LN-PURDATE   AERN-LN-LOANDATE
      *      AERN-LN-INTDATE      AERN-LN-TOTNODEF  AERN-LN-ANTICTERM
      *      AERN-LN-SALEFIN      AERN-LN-ANTICERN  AERN-LN-ANTICADJ
      *      AERN-LN-ACCERN       AERN-LN-UFERN     AERN-LN-REBATE
      *
      *  CURRENTLY THIS LOGIC FOR ADDON WILL ONLY WORK FOR FORMULA 51:
      *
      *   OUT : AERN-EARNINGS(AERN 1 TO 6)   COMPONENT EARNINGS FROM ACCRUAL
      *         AERN-ERNDELQ-STOP            USED FOR SETTING LOAN RECORD
      *         AERN-NEW-LAST-ACCRUAL-DATE   THE ACCRUAL DATE USED FOR THIS LOAN
      *                                      RE: SP-ERNDELQFRMLA 'C'
      *
      *   USED:
      *   COPY:
      *  REV :
      *   070987 AMW CHANGE ACCRUAL 78THS EARNINGS FORMULA TO
      *               WORK ON MIDSTREAM PURCHASES.
      *   073187 AMW CHANGE "O" AND "P" FORMULAS TO USE NEW
      *              SP-ERNACCRL-FG TO DETERMINE WHEN TO
      *              START TAKING ACCRUAL EARNINGS.
      *   102387 JTG CHANGED "L" AND "M" FORMULAS TO
      *              PROPERLY HANDLE ACTUARIAL EARNINGS
      *              WITH UPFRONT EARNINGS.
      *   120187 JTG CHANGED "L" AND "M" FORMULAS TO SET
      *              REB-SKIPDEF-FG FOR ACTUARIAL REBATE.
      *   010788 JTG ADDED NEW LOGIC FOR FORMULA 'K'
      *   022688 JPN ADDED NEW LOGIC FOR FORMULA 'T'
      *   030888 JTG MODIFIED LOGIC FOR 'O' AND 'P'
      *              TO ALWAYS ADJUST EARNINGS TO EXACTLY
      *              WHAT IT SHOULD BE.
      *   032388 JTG ADDED LOGIC TO 'O' AND 'P' TO ALLOW
      *              THE 78THS PORTION TO HAVE DEFERMENTS
      *              IMPACT THE EARNINGS FOR A PERIOD
      *   042288 BAW CHANGED ALL LETTERS TO NUMBERS
      *   042288 BAW CHANGED ALL SP- TO SP-(1)
      *   061388 JTG ADDED LOGIC FOR #21
      *   032789 JTG ADDED LOGIC FOR #22
      *   041989 JTG ADDED FORMULA 59 (IDEAL)
      *   051789 JTG ADED LOGIC FOR SP-ERRNACCRL-FG - C
      *              FORMULA 15 16 AND 21
      *   053189 JTG ADDED LOGIC FOR DELIQ STOP FORMULA (PIONEER)
      *   060689 SLC CHANGED FORMULA 15 TO 51 AND 16 TO 52.
      *   102589 JTG FIXED BUG IN RECALCULATION OF APR
      *   110289 SLC ADDED LOGIC FOR TFC DELINQUENT STOP,
      *              CONTINUE CASH EARNINGS FOR REMAINDER OF TERM.
      *   112189 JTG CORRECTED ACTUARIAL FRMLA 52 TO USE SP-ERN-DAYS
      *              AND THE INTEREST START DATE
      *   112989 SLC NOW USE DISCLOSED-FIINAMT INSTEAD OF FINANCED-AMT
      *              FOR COMPUTING APR.
      *   121189 JTG/SLC OPENED UP ACTUARIAL FOR DEALER DISCOUNT
      *   122089 SLC IF CURBAL IS 0, DON'T ACCRUE!!
      *   122289 JTG FIXED BUG RE: EXIT TAKING INCOME ON CASH METHODS
      *   032990 JTG STOPPED PL AND SA FROM TAKING INCOME
      *   041090 JTG CHANGED 52 TO CALC APR ON ALL SALEFIN ACCTS
      *   050290 JTG PUT TEST ON LN-REBATE() TO SKIP COMPONENT
      *   061890 JTG STOPPED DIR LOANS FROM BEING VIEWED AS MIDSTREAM
      *   062590 JTG MADE ACTUARIAL FOR COMPONENTS TO PRORATE AGAINST
      *              TOTAL CHARGE ACTUARIAL REBATE
      *   072690 JTG ADDED LOGIC FOR FORMULA 55, RESTORING LOGIC FOR
      *              PRIOR VERSION OF 52
      *   091990 SLC ADDED FIX TERM OPTION FOR FORMULA 53.
      *   122690 JTG ALLOWED GA SCHG TO BE EARNED VIA 59
      *   020991 JTG ADDED SP-ERNACCRL-START-CD() = "B" AND "A"
      *   022091 JTG CORRECT EARNINGS FOR "X" (MULL ) CLASS 70'S
      *   081892 JTG ADDED 56 RULE 78 CASH METHOD, AS OF PAYMENT
      *              POST DATE
      *   050693 BAH CHANGED LN-PAYFREQ TO LN-UNITPER-CD AND LN-UNITPER-FREQ
      *   012794 JTG ADDED LOGIC FOR ERNFRMLA = 13, DISCOUNT YIELD - INT
      *   060294 JTG CORRECTED FOR CITIZENS, RE: MIDSTREAM PURCHASE
      *              AND ALL EARNINGS ARE PUT INTO DISCOUNT.
      *   021095 JTG ADDED LOGIC FOR SP-ERNFRMLA 52  TO SKIP DEFERED PERIODS
      *   060596 JTG FIXED BUG, WHEN ACCOUNTS WERE ACCELERATED AND INTEREST HAD
      *              BEEN REBATED, AERN-LN-REBATE WAS SET AND REMAINED SET.
      *              WHEN EARNINGS FOR DISCOUNT WAS DONE, CAUSING NO DISCOUNT
      *              EARNINGS TO BE TAKEN.
      *   121896 JTG FIXED BUG, WHEN ACCOUNTS WERE EITHER:
      *                     1. ACCELERATED AND SP-ERNACCEL-CD = " "
      *                     2. PUT IN JUDGEMENT AND SP-ERNJD-CD = " "
      *              THAN ALL EARNING COMPONENTS WERE CLOSED OUT.
      *              HOWEVER, THE BUG DID'T RECONIZE THIS AND CONTINUED TO
      *              EARN ALL NON REBATED COMPONENTS.
      *   071197 JTG ADDED SP-ERNFRMLA = 64, MERCURY DISCOUNT EARN FORMULA
      *              SAME RATIO AS INTEREST IS EARNED
      *   093097 JTG CHANGED TO TAKE UP FRONT EARNINGS INTO ACCOUNT
      *   011597 JTG ALLOWED FORMULA 62 & 63 TO WORK FROM A50, MID ATLANTIC
      *   011998 JTG ALLOWED OTHER THAN MONTHLY PAPER TO WORK FOR  51,53 & 56:
      *   021098 JTG ADDED SET OF REB-EARN-FULL-MONTH, TFC #637
      *              RE: SP-EARNACCRL-FG() = 'B'
      *   030398 JTG CORRECTED COMPUTE OF DISCLOSED-FINAMT, RE: LASTPYAMT
      *              IN AERN-COMPUTE-COMPONENT-APR ROUTINE
      *   041398 JTG MADE AERN-MIDSTREAM-NEW-PUR-AFTER A SEC.TION
      *   071398 JTG CHANGED SP-ERNFRMLA 52 & 55 TO CORRECTLY HANDLE MIDSTREAMS
      *              RE: NON MONTHLY ROUTINES
      *              AND PUT BACK LOGIC TO CALL REBATE
      *   080798 JTG CHANGED SP-ERNFRMLA 52 & 55 TO MATCH OLD CALCULATIONS
      *              ON NON MIDSTREAM PAPER
      *   081298 JTG ESTABLISHED FORMULA 60 TO MATCH 52 ON 080798, FOR MERCURY
      *   081398 JTG PER CHANGE ON 081298 FOR MERCURY ESTABLISHED IDENTICAL
      *              ROUTINE FOR FORMULA 60 AERN-MIDSTREAM-60
      *   081498 JTG MODIFIED FORMULA 64 TO WORK WHEN ACCOUNT IS 'IB' BY
      *              VIEWING 'IB' ACCOUNT AS IF IT WAS 'PC' AND DETERMINING
      *              EARNINGS FOR PERIOD VIA ERNFRMLA = 60
      *   091698 JTG MODIFIED FORMULA 64 TO USE ERNFRMLA 51, RULE 78THS WHEN
      *              THE INTEREST CHARGE IS ZERO
      *   092898 JTG CHANGED FORMULA 64 TO WORK FOR OTHER THAN INITIAL SLICE
      *   101298 JTG CHANGED FORMULA 64 TO DO RULE 78THS WHEN ACCOUNT IS
      *              'IB' MIDSTREAM
      *   990326 JTG ADDED AERN-LAST-ACCRUAL-DATE AERN-NEW-LAST-ACCRUAL-DATE
      *              RE: SP-ERNDELQFRMLA 'C' TFC #005
      *   990331 JTG CHANGED ROUTINE TO TEST LAST PAY DATE OF DELINQUENCY
      *              PROFILE IS 7               RE: SP-ERNDELQFRMLA 'C' TFC #005
      *   990614 JTG CORRECTED FORMULA 63 TO CAP EARNINGS TO NOT EXCEED UNEARNED
      *   990809 JTG CORRECTED FORMULA 63 TO NOT TAKE NEGITIVE EARNINGS
      *              RE: FULL UPFRONT TAKEN FOR DISCOUNT
      *   JTG 000204 FIXED BUG TO INCLUDE BUCKET 6, WHEN ACCOUNTS WERE EITHER:
      *                     1. ACCELERATED AND SP-ERNACCEL-CD = " "
      *                     2. PUT IN JUDGEMENT AND SP-ERNJD-CD = " "
      *              THAN ALL EARNING COMPONENTS WERE CLOSED OUT.
      *              HOWEVER, THE BUG DID'T RECONIZE THIS AND CONTINUED TO
      *              EARN ALL NON REBATED COMPONENTS.
      *  JTG 000406 ADDED NEW SP-ERNACCRL-FG() = D, NEW SOUTH #1129
      *             EARN AFTER SAME AS CASH PERIOD
      *  JTG 000714 CHANGED RE: BETH'S CONVERSIONS TO TAKE NO INCOME WHEN
      *             LN-UFERN() = LN-ACCERN()
      *  JTG 000801 CORRECTED RE: BETH'S CONVERSIONS TO TAKE NO INCOME WHEN
      *             LN-UFERN() = LN-ACCERN() AND LN-ACCERN() = LN-ANTICERN()
      *             AND LN-UFERN() NOT = 0
      *  JTG 021098 ADDED ELSE TO SET OF REB-EARN-FULL-MONTH TO ALLOW CLEAR
      *             OF SET REB-EARN-FULL-MONTH, TFC #637
      *             RE: SP-ERNACCRL-FG() = 'B'
      *  JTG 010705 ADDED SET OF REB-EARN-FULL-MONTH,       REGENCY PR#1456
      *              RE: SP-ERNACCRL-FG() = 'C'
      *  JTG 020104 CORRECTED CHANGE FOR DAILY ACCRUALS TO NOT FORSE LAST
      *             DAY OF MONTH WHEN MOVING THE ACCRUAL DATE BACK BY THE NO.
      *             OF MONTHS DEFERRED, BY TESTING FOR AN ACCRUAL DATE WHICH
      *             IS THE LAST DAY OF A MONTH.
      *             PR#245 MOVE 40 TO NDTE-DD  ALLOW DAILY ACCRUALS TO WORK
      *                                                            REGWAN PR#245
      * JTG 040601 ADDED GL CONTROL FIELDS FOR:
      *                - UNEARNED INTEREST REPO
      *                - UNEARNED ADDON INTEREST REPO
      *            THE UNEARNED WILL MOVE TO THESE ACCOUNTS POSTING A 'SX'
      *            AND REVERSING A 'SV'
      *            THE UNEARNED WILL MOVE (BACK) FROM THESE ACCOUNTS POSTING
      *            A 'SV' AND REVERSING A 'SX'
      *
      *          NOTE:
      *            ACCRUAL EARNINGS WILL STOP WHEN ACCOUNTS ARE IN REPO
      *            STATUS "X" ONHAND AND "S" SOLD                  REGACC #0049
      * JTG 050225 CORRECTED BUG IN USE OF SP-ERN-TERM() FOR SP-ERNFRMLA 53
      *            LOGIC IN AERN-PRORATA SECTION WAS OVERRIDING
      *            AERN-LN-ANTICTERM WITH SP-ERN-TERM(). LOGIC NOW RESETS THE
      *            ORIGINAL VALUE OF AERN-LN-ANTICTERM.            REGWAN
      * JTG 050712 ADDED CHANGES FOR REPORTING AND TAKING FEE CODE EARNINGS
      *            CREATED CD-FEE-EARNINGS-FG                     LENDMARK #2542
      * JTG 050920 ALLOWED EARN FORMULA '52' AND '60' TO USE
      *            SP-ERNACCRL-FG = 'D'.
      *            WHEN AN ACCOUNT IS UNDER SAME AS CASH (LN-CASHDATE NOT = 0)
      *            THE 1ST ACCRUAL WILL BE TAKEN AT THE END OF THE MONTH THE
      *            SAC PERIOD EXPIRES.                            LENDMARK 2553
      * JTG 051025 STARRED OUT LOGIC TO SUPRESS CLEAR OF AERN-EARNINGS(1)
      *            SINCE ADDON SPR EARNINGS MUST BE FORMULA 51 AND WHEN
      *            THERE ARE NO ADDONS THE THE LOGIC PERFORMS AERN-64-VIA60
      *            WHICH DOESN'T NEED PRIOR COMPUTED INTEREST EARNINGS
      *                                                          LENDMARK 2542
      *  CS 121119 WORLD WANTED TO RE-CALCULATE EARNINGS ON A CASH METHOD ONCE
      *            ACCOUNT REACHES SP-ERNDELQ-STOP CONTRACTUAL RECALCULATE
      *            ACCRUAL EARNINGS 52 ON CASH 6.  FORCED A RECALCULATE OF
      *            EARNINGS DATE (AERN-ENDDATE) BASED ON RETRIEVING A DATE
      *            THAT EARNINGS ARE PAID THRU VIA PAYMENTS MADE. THIS DATE IS
      *            USED TO ACCRUE EARNINGS(SHOULD BE NEGATIVE), THEN THE LOAN
      *            RECORD IS UPDATED WITH A 'C' IN LN-ERNDELQ-STOP, WHICH WILL
      *            STOP FURTHER ACCRUALS AND THE LOAN WILL BE IN A CASH EARNING
      *            METHOD FOR ALL TIME.  WORLD PR#828
      *  CS 120925 ALLOW SP-ERNFRMLA( ) 65, IF 65 PERFORM COMPUTE-EARNINGS-
      *            PAYMENTS-CASH WHICH CALCULATES A NEW DATE BASED ON PAYMENTS
      *                                               WRLD PR#930
      *  CS 120926 WAS ONLY TESTING ERNFRMLA(1) = 65,NEEDS TO BE ERNFRMLA(1),
      *            SP-ERNFRMLA(3), AND SP-ERNFRMLA(4)  WRLD PR930
      *  CS 131022 I WAS FORCING 52 TO SP-ERNFRMLA SO IT WOULD FOLLOW 52 LOGIC,       *            BUT WE ONLY READ THE SPR IF THE KEY CHANGES SO THE NEXT
      *            LOANS EARNINGS FORMULA WAS SITTING AT 52. ONCE I FIXED THIS
      *            I WAS NOT SETTING AERN-NEW-LAST-ACCRUAL-DATE TO AERN-ENDDATE
      *                                                WORLD PR#930
      * BAH 2021.1026 ACCOUNTS WITH 0 BALANCE WERE EXITING ROUTINE WITH GARBAGE
      *               IN AERN-NEW-LAST-ACCRUAL. DEFAULTED BEFORE EXIT.
      * BAH 2024.0805 THE ABOVE CHANGE WAS EXITING P&L AND PAIDOUTS WITH
      *               THE EOM DATE(AERN-ENDDATE) AND SHOULD BE EXITING WITH
      *               THE ORIGINAL LN-LAST-ACCRUAL-DATE AS IT IS NOT TAKING
      *               ANY EARNINGS. S35Q-100
      **************************************************************************
       COMPUTE-AERNINGS SECTION.

           MOVE 0 TO AERN-EARNINGS(1).
           MOVE 0 TO AERN-EARNINGS(2)
                     AERN-EARNINGS(3) AERN-EARNINGS(4)
                     AERN-EARNINGS(5) AERN-EARNINGS(6).

           MOVE 0 TO     AERN-CERN-PAYMNTD
                         AERN-CERN-PAYMENTS
                         AERN-CERN-SAVE-PAYMNTD
                         AERN-CERN-TOTLCHG
                         AERN-CERN-WKDATE1
                         AERN-CERN-SAVE-WKDATE1
                         AERN-CERN-SAVE-WPAYS.

           MOVE LN-ERNDELQ-STOP TO AERN-ERNDELQ-STOP.

      * THIS NEEDS TO SET AERN-NEW-LAST-ACCRUAL-DATE TO THE ORIGINAL
      * LOAN LN-LAST-ACCRUAL-DATE FOR WHEN IT EXITS WITH ALL THE BELOW 
      * CONDITIONS WITHOUT SETTING THE NEW DATE, AS NO ACCRUAL WILL TAKE
      * PLACE
           MOVE LN-LAST-ACCRUAL-DATE TO AERN-NEW-LAST-ACCRUAL-DATE.

           IF ( AERN > 6          ) OR
              ( LN-CURBAL = 0     ) OR
              ( LN-PLCD NOT = " " ) OR
              ( LN-SERVACCT = "Y" ) OR
              ( ( LN-REPOCD = "X" OR "S" ) AND
                ( GP-NEW-REPO-UNEARNED-ACCTS = "Y" )
              )
                GO TO COMPUTE-AERNINGS-EXIT.

      *
      * THIS SECTION OF CODE IS FOR ESTABLISHMENT OF ORIG LOAN FIELDS.
      * WHEN AERN-ADDON-FG = "Y", IT IS ASSUMED THAT THE FOLLOWING FIELDS
      *      HAVE BEEN PRESET TO THE CORRECT VALUES FOR THE ADDON.
      *      IF NOT = 'Y' SET AERN FIELDS FROM LOAN RECORD.
      *
           IF AERN-ADDON-FG NOT = "Y"
              MOVE LN-ORIG-1STPYDATE TO AERN-LN-ORIG-1STPYDATE
              MOVE LN-PURDATE TO AERN-LN-PURDATE
              MOVE LN-LOANDATE TO AERN-LN-LOANDATE
              MOVE LN-INTDATE TO AERN-LN-INTDATE
              MOVE LN-TOTNODEF TO AERN-LN-TOTNODEF
              MOVE LN-ANTICTERM TO AERN-LN-ANTICTERM
              MOVE LN-SALEFIN TO AERN-LN-SALEFIN.

      *
      

           IF SP-ERNDELQFRMLA = " "
      * SPR INDICATE THAT EARNING STOPS ARE NOT DONE
      * SO ACCRUAL WILL RUN THRU AERN-ENDDATE
              MOVE AERN-ENDDATE TO AERN-NEW-LAST-ACCRUAL-DATE
           ELSE
              MOVE AERN-ENDDATE TO AGEING-DATE
              PERFORM COMPUTE-CONTRACTUAL
              PERFORM AERN-ERNDELQ-STOP-START-TEST
              IF AERN-ERNDELQ-STOP = "Y" OR "C"
      * ACCOUNT WAS STOPPED AND IS STILL STOPPED
      * SO NO ACCRUAL WILL WILL TAKE PLACE
      * RESET LAST ACCRUAL DATE TO VALUE PRIOR TO THIS UPDATE
      *   (ORIGINAL LN-LAST-ACCRUAL-DATE)
                 MOVE
                 AERN-LAST-ACCRUAL-DATE TO AERN-NEW-LAST-ACCRUAL-DATE
                 GO TO COMPUTE-AERNINGS-EXIT
              ELSE
              IF AERN-ERNDELQ-STOP = "T"
      * ACCOUNT WAS STOPPED AND IS STILL STOPPED
      * BUT IN A MODE TO TAKE AN ACCRUAL THRU A DATE
      * DETERMINED BY PAYMENTS MADE
                 MOVE AERN-ENDDATE TO AERN-NEW-LAST-ACCRUAL-DATE
                 MOVE "Y" TO AERN-ERNDELQ-STOP
              ELSE
              IF AERN-ERNDELQ-STOP = "R"
                PERFORM AERN-EARNINGS-PAYMENTS
                MOVE AERN-CERN-WKDATE1 TO AERN-ENDDATE
                            AERN-NEW-LAST-ACCRUAL-DATE
              ELSE
      * IF AERN-ERNDELQ-STOP = " "
      * ACCOUNT WAS STOPPED AND HAS COME OUT OF STOP STATUS
      * SO ACCRUAL WILL RUN THRU AERN-ENDDATE
                 MOVE AERN-ENDDATE TO AERN-NEW-LAST-ACCRUAL-DATE.

           IF SP-ERNFRMLA(1) = 65
             OR SP-ERNFRMLA(3) = 65
               OR SP-ERNFRMLA(4) = 65
                PERFORM AERN-EARNINGS-PAYMENTS
                MOVE AERN-ENDDATE TO AERN-NEW-LAST-ACCRUAL-DATE.

           IF AERN = 0
              MOVE 1 TO AERN
              PERFORM COMPUTE-AERNINGS-COMPONENT 6 TIMES
           ELSE
              PERFORM COMPUTE-AERNINGS-COMPONENT.

       COMPUTE-AERNINGS-EXIT.
           MOVE " " TO AERN-ADDON-FG.
           MOVE 0 TO AERN.

      ******************************************************
      *    GET A COMPONENT'S ACCRUAL EARNINGS
      ******************************************************
       COMPUTE-AERNINGS-COMPONENT SECTION.
           MOVE 0 TO AERN-EARN.
           IF ( SP-ERNFRMLA(AERN) < 51                            ) OR
              ( AERN-ADDON-FG NOT = "Y" AND LN-ANTICERN(AERN) = 0 ) OR
              ( AERN-ADDON-FG = "Y" AND AERN-LN-ANTICERN = 0      ) OR
              ( ( LN-UFERN(AERN) NOT = 0                   ) AND
                ( LN-UFERN(AERN) =
                     LN-ANTICERN(AERN) + LN-ANTICADJ(AERN) ) AND
                ( LN-UFERN(AERN) = LN-ACCERN(AERN)         )
              )
              GO TO COMPUTE-AERNINGS-COMP-END.

           IF ( AERN = 2             ) AND
              ( AERN-ADDON-FG = " "  ) AND
              ( AERN-LN-ANTICERN = 0 )
              GO TO COMPUTE-AERNINGS-COMP-END.

      * THIS SECTION OF CODE IS FOR ESTABLISHMENT OF ORIG LOAN FIELDS.
      * WHEN AERN-ADDON-FG = "Y", IT IS ASSUMED THAT THE FOLLOWING FIELDS
      *      HAVE BEEN PRESET TO THE CORRECT VALUES FOR THE ADDON.
      *
           IF AERN-ADDON-FG NOT = "Y"
              IF AERN = 1
      * LOGIC FOR MULLEN, CA CONVERTED ACCOUNTS:
                 IF LN-CONVERCD = "X"
                    IF (LN-CLASS NOT = 70)
                       OR (LN-CLASS = 70
                            AND (LN-ANTICERN(1) = LN-UFERN(1)))
                       MOVE LN-OLDACCTNO TO AERN-LN-PURDATE
                                            AERN-LN-INTDATE
                                            AERN-LN-LOANDATE
                                            NDTE-DATE
                       MOVE LN-PRLNNO TO AERN-LN-ANTICTERM
                       MOVE 1 TO NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE TO AERN-LN-ORIG-1STPYDATE.

           IF AERN-ADDON-FG NOT = "Y"
              MOVE LN-UFERN(AERN) TO AERN-LN-UFERN
              IF AERN = 1
                 IF LN-CONVERCD = "X"
                    IF LN-CLASS = 70
                       IF LN-ANTICERN(1) = LN-UFERN(1)
                          NEXT SENTENCE
                       ELSE
                          MOVE 0 TO AERN-LN-UFERN.

           IF AERN-ADDON-FG NOT = "Y"
              MOVE LN-ANTICERN(AERN) TO AERN-LN-ANTICERN
              MOVE LN-ANTICADJ(AERN) TO AERN-LN-ANTICADJ
              MOVE LN-ACCERN(AERN)   TO AERN-LN-ACCERN
              IF AERN = 1
                 MOVE LN-REBATE(4) TO AERN-LN-REBATE
              ELSE
                 IF AERN = 3
                    MOVE LN-REBATE(5) TO AERN-LN-REBATE
                 ELSE
                    IF AERN = 4
                       MOVE LN-REBATE(6) TO AERN-LN-REBATE
                    ELSE
                       MOVE " " TO AERN-LN-REBATE.

      * SKIP REBATED COMPONENTS
      *       OR ACCOUNTS WHICH WERE ACCELERATED OR PUT IN JUDGEMENT
      *            AND SPRS INDICATED TAKING ALL REMAINING UNEARN
      *                INTO INCOME:

           IF ( ( AERN-LN-ANTICADJ NOT = 0                      ) OR
                ( NOT (AERN-LN-REBATE = " " OR "C")             ) OR
                ( LN-ACCELDATE NOT = 0 AND SP-ERNACCEL-CD = " " ) OR
                ( LN-JDDATE NOT = 0 AND SP-ERNJD-CD = " "       )
              )
              GO TO COMPUTE-AERNINGS-COMP-END.

      * SELECT APPROPRIATE EARNINGS FORMULA:
           IF SP-ERNFRMLA(AERN) = 51
              PERFORM AERN-RULE78
           ELSE
           IF SP-ERNFRMLA(AERN) = 52 OR 55 OR 65
              PERFORM AERN-ACTUARIAL-52-55-65
           ELSE
           IF SP-ERNFRMLA(AERN) = 53
              PERFORM AERN-PRORATA
           ELSE
           IF SP-ERNFRMLA(AERN) = 54
              PERFORM AERN-RULE78-5
           ELSE
           IF SP-ERNFRMLA(AERN) = 56
              PERFORM AERN-RULE78-CASH
           ELSE
           IF SP-ERNFRMLA(AERN) = 57
              PERFORM AERN-ACTUARIAL-57
           ELSE
           IF SP-ERNFRMLA(AERN) = 58
              PERFORM AERN-LESSER-78
           ELSE
           IF SP-ERNFRMLA(AERN) = 59
              PERFORM AERN-REBATE
           ELSE
           IF SP-ERNFRMLA(AERN) = 60
              PERFORM AERN-ACTUARIAL-60
           ELSE
      *    IF SP-ERNFRMLA(AERN) = 61
      *       NEXT SENTENCE
      *    ELSE
           IF SP-ERNFRMLA(AERN) = 62
              PERFORM AERN-ACTUARIAL-62
           ELSE
           IF SP-ERNFRMLA(AERN) = 63
              PERFORM AERN-ACTUARIAL-63
           ELSE
           IF SP-ERNFRMLA(AERN) = 64
              PERFORM AERN-64-VIA60
           ELSE
              GO TO COMPUTE-AERNINGS-COMP-EXIT.

      * TEST FOR TOTAL EARNINGS TAKEN GREATER THAN ANTICIPATED:
            IF AERN-EARN > AERN-LN-ANTICERN
               COMPUTE AERN-EARN = AERN-LN-ANTICERN.

            IF SP-ERNFRMLA(AERN) = 62 OR 63
               MOVE AERN-EARN TO AERN-EARNINGS(AERN)
               GO TO COMPUTE-AERNINGS-COMP-END.

            IF AERN-EARN < 0
               MOVE 0 TO AERN-EARN.

       COMPUTE-AERNINGS-COMP-EXIT.
           ADD AERN-EARN TO AERN-EARNINGS(AERN).
           COMPUTE AERN-EARNINGS(AERN) = AERN-EARNINGS(AERN)
                                                - AERN-LN-ACCERN.
       COMPUTE-AERNINGS-COMP-END.
           ADD 1 TO AERN.


       AERN-EARNINGS-PAYMENTS SECTION.
           MOVE LN-TOTLCHG   TO AERN-CERN-TOTLCHG.
           ADD  LN-TOTPAYMNTD LN-TOTEXCPAYMNTD TO
                                          AERN-CERN-SAVE-PAYMNTD
                                          AERN-CERN-PAYMNTD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE "A" TO SP-DISFRMLA.

      * SETUP UNIT PERIOD INFORMATION:
           MOVE LN-ORGTERM TO UPER-TERM.
           MOVE LN-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE LN-INTDATE TO UPER-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO UPER-1STPYDATE.
           PERFORM UPER-CALCULATION.

           IF SP-CONTRFRMLA = "A"
              SUBTRACT AERN-CERN-TOTLCHG FROM AERN-CERN-PAYMNTD.
           PERFORM COMPUTE-EARNINGS-PAYMENT-CASH.
           MOVE NDTE-DATE TO AERN-CERN-WKDATE1.
           MOVE AERN-CERN-WPAYS TO AERN-CERN-SAVE-WPAYS.
           MOVE AERN-CERN-WKDATE1 TO AERN-ENDDATE.

      ************************************
      * COMPUTE EARNING PAYMENTS
      ************************************
       COMPUTE-EARNINGS-PAYMENT-CASH SECTION.
           IF AERN-CERN-PAYMNTD NOT > LN-1STPYAMT
              COMPUTE AERN-CERN-PAYMENTS ROUNDED =
                               AERN-CERN-PAYMNTD / LN-1STPYAMT
              MOVE LN-INTDATE   TO NUM-DATE
              MOVE LN-1STPYDATE TO SYS-DATE
              PERFORM TIM365
              MULTIPLY AERN-CERN-PAYMENTS BY ELAPSED-DAYS
                             GIVING NDTE-HOLD ROUNDED
              MOVE LN-INTDATE TO NDTE-DATE
              PERFORM INCREMENT-DAYS
           ELSE
           IF AERN-CERN-PAYMNTD NOT > (LN-1STPYAMT
                                       + LN-NOREGPY * LN-REGPYAMT)
              COMPUTE AERN-CERN-PAYMENTS ROUNDED =
                 (AERN-CERN-PAYMNTD - LN-1STPYAMT) / LN-REGPYAMT
              MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
              MOVE AERN-CERN-WPAYS TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MULTIPLY AERN-CERN-PPAYS BY UPER-DAYS-IN-UNITPER
                             GIVING NDTE-HOLD ROUNDED
              PERFORM INCREMENT-DAYS
           ELSE
              COMPUTE AERN-CERN-PAYMENTS ROUNDED =
                  ( AERN-CERN-PAYMNTD
                       - (LN-1STPYAMT + LN-NOREGPY * LN-REGPYAMT)
                  )
                    / LN-LASTPYAMT
              MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
              MOVE LN-NOREGPY TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MULTIPLY AERN-CERN-PPAYS BY UPER-DAYS-IN-UNITPER
                             GIVING NDTE-HOLD ROUNDED
              PERFORM INCREMENT-DAYS.

       COMPUTE-EARNINGS-PAYMENTS-CASH-EXIT.
           EXIT.

      *********************************************************
      *                   (51)
      *    COMPUTE EARNINGS RULE78 METHOD:
      *
      *********************************************************
       AERN-RULE78 SECTION.

      *    LOANS BOOKED, WITH FEES FLAGGED WITH CD-FEE-EARNINGS-FG = 'Y',
      *    ACCUMULATE AND STORE THE FEE AMOUNTS IN THE ADDON ANTICIPATED
      *    EARNINGS BUCKET.
      *    THE ONLY VALID EARN FORMULA FOR ADDON ANTICIPATED IS '51',
      *    WHICH, AS SEEN BELOW, HAS BEEN CHANGED TO TEST FOR ADDON
      *    ANTICIPATED WITH (NO) LOAN ADDONS AND EARN THIS ANTICIPATED
      *    UNEARNED VIA AERN-64-VIA60 WHICH EARNS IN THE SAME RATIO OF
      *    EARNINGS AS INTEREST.
      *
      *     SAME RATIO OF EARNINGS AS INTEREST IS EARNED VIA: ERNFRMLA 60
      *
      *           ...WHEN THE INTEREST CHARGE IS ZERO(0) OR ACCOUNT IS
      *              'IB' MIDSTREAM, EARNINGS WILL BE COMPUTED VIA
      *              SP-ERNFRMLA 51 (RULE 78THS)
      *
      *           ...WHEN ACCOUNT IS 'IB' ACCOUNT IS VIEWED AS IF IT
      *              WAS 'PC' AND EARNINGS FOR PERIOD IS DETERMINED
      *              VIA ERNFRMLA = 60
      *
      *
           IF ( AERN = 2             ) AND
              ( AERN-ADDON-FG = " "  )
              PERFORM AERN-64-VIA60
              GO TO AERN-RULE78-EXIT.
      
           PERFORM AERN-RULE78-LOGIC.

       AERN-RULE78-EXIT.
           EXIT.

      *****************************************************
      *    RULE78 LOGIC
      *****************************************************
       AERN-RULE78-LOGIC SECTION.
           MOVE 0 TO AERN-WORKER.
           SUBTRACT AERN-LN-UFERN FROM AERN-LN-ANTICERN
                                    GIVING AERN-ANTICIPATED.
           MOVE AERN-LN-UFERN TO AERN-EARN.

           PERFORM AERN-GET-REM-TERM.

           IF AERN-REMTERM NOT < 0
              IF AERN-REMTERM NOT > AERN-LN-ANTICTERM
                 COMPUTE AERN-EARN ROUNDED = AERN-EARN +
                    (1 - (AERN-REMTERM * (AERN-REMTERM + 1))
                    / ((AERN-LN-ANTICTERM) *
                      ((AERN-LN-ANTICTERM) + 1)))
                    * AERN-ANTICIPATED.

      * IF PAST LAST MONTH TO TAKE EARNINGS ON, TAKE WHAT IS LEFT.
           IF AERN-REMTERM < 0
              MOVE AERN-LN-ANTICERN TO AERN-EARN.

       AERN-RULE78-LOGIC-EXIT.
           EXIT.

      ****************************************************
      *                (52 OR 55)
      *    COMPUTE EARNINGS ACTUARIAL METHOD:
      *
      *    52 - CALCULATE NEW APR'S FOR COMPONENTS
      *    55 - PRORATE COMPONENTS TO REFUND ON TOTAL CHG
      *    65 - USES PAYMENTS PAID TO SET THE ACCRUAL DATE AND THEN
      *         COMPUTES EARNINGS USING FORMULA 52
      ****************************************************
       AERN-ACTUARIAL-52-55-65 SECTION.
           PERFORM AERN-SAVE-LN-SP.
           MOVE "A" TO SP-DISFRMLA.

      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
           PERFORM AERN-SET-WORKERS-NEW.

           IF SP-ERNFRMLA(AERN) = 52
              IF (SP-ERNACCRL-FG(AERN) = "D") AND
                 (LN-CASHDATE NOT = 0       )
                 MOVE LN-CASHDATE  TO NUM-DATE
                 MOVE AERN-ENDDATE TO SYS-DATE
                 IF SYS-DATE < NUM-DATE
                    MOVE 0 TO AERN-EARN
                    GO TO AERN-ACTUARIAL-52-55-65-EXIT
                 END-IF
              END-IF
              IF (AERN-LN-SALEFIN = "Y") AND
                 (LN-ORIGPURAMT NOT = LN-LNAMT)
                 PERFORM AERN-MIDSTREAM-NEW
                 PERFORM AERN-APR-COMPONENT-NEW
                 PERFORM AERN-ACTUARIAL-COMPONENT
              ELSE
                 IF (AERN = 1 AND
                      LN-ANTICERN(2) = 0 AND
                       LN-ANTICERN(3) = 0 AND
                        LN-ANTICERN(4) = 0 AND
                         (LN-DLDISC = 0
                               OR LN-ANTICERN(5) NOT = 0) AND
                          LN-UFERN(1) = 0 AND
                           AERN-LN-SALEFIN = "N")
                    PERFORM AERN-ACTUARIAL-COMPONENT
                 ELSE
                    PERFORM AERN-APR-COMPONENT-NEW
                    PERFORM AERN-ACTUARIAL-COMPONENT.


           IF SP-ERNFRMLA(AERN) = 65
              IF (SP-ERNACCRL-FG(AERN) = "D") AND
                 (LN-CASHDATE NOT = 0       )
                 MOVE LN-CASHDATE  TO NUM-DATE
                 MOVE AERN-ENDDATE TO SYS-DATE
                 IF AERN-ENDDATE < LN-CASHDATE
                    MOVE 0 TO AERN-EARN
                    GO TO AERN-ACTUARIAL-52-55-65-EXIT
                 END-IF
              END-IF
              IF (AERN-LN-SALEFIN = "Y") AND
                 (LN-ORIGPURAMT NOT = LN-LNAMT)
                 PERFORM AERN-MIDSTREAM-NEW
                 PERFORM AERN-APR-COMPONENT-NEW
                 PERFORM AERN-ACTUARIAL-COMPONENT
              ELSE
                 IF (AERN = 1 AND
                      LN-ANTICERN(2) = 0 AND
                       LN-ANTICERN(3) = 0 AND
                        LN-ANTICERN(4) = 0 AND
                         (LN-DLDISC = 0
                               OR LN-ANTICERN(5) NOT = 0) AND
                          LN-UFERN(1) = 0 AND
                           AERN-LN-SALEFIN = "N")
                    PERFORM AERN-ACTUARIAL-COMPONENT
                 ELSE
                    PERFORM AERN-APR-COMPONENT-NEW
                    PERFORM AERN-ACTUARIAL-COMPONENT.


           IF SP-ERNFRMLA(AERN) = 55
              IF (AERN-LN-SALEFIN = "Y")
                     AND (LN-ORIGPURAMT NOT = LN-LNAMT)
                 PERFORM AERN-MIDSTREAM-NEW
                 PERFORM AERN-APR-COMPONENT-NEW
                 PERFORM AERN-ACTUARIAL-COMPONENT
              ELSE
                 IF AERN = 1
                    IF LN-DLDISC = 0 OR LN-ANTICERN(5) NOT = 0
                       PERFORM AERN-COMPUTE-ALLCHG-APR
                       PERFORM AERN-ACTUARIAL-PRORATA
                    ELSE
                       PERFORM AERN-APR-COMPONENT-NEW
                       PERFORM AERN-ACTUARIAL-COMPONENT
                 ELSE
                    PERFORM AERN-COMPUTE-ALLCHG-APR
                    PERFORM AERN-ACTUARIAL-PRORATA.

       AERN-ACTUARIAL-52-55-65-EXIT.
           PERFORM AERN-RESTORE-LN-SP.

      *****************************************************
      *    (52) & (55)
      *    ACTUARIAL COMPONENT ROUTINE
      *****************************************************
       AERN-ACTUARIAL-COMPONENT SECTION.
           SUBTRACT AERN-LN-UFERN FROM AERN-LN-ANTICERN
                                    GIVING AERN-WK-FINCHG.
           PERFORM AERN-ACTUARIAL-REBATE-NEW.
           COMPUTE AERN-EARN = AERN-LN-ANTICERN - REB-REBATE.

      *****************************************************
      *    (52) & (55)
      *    ACTUARIAL PRORATA ROUTINE
      *****************************************************
       AERN-ACTUARIAL-PRORATA SECTION.
      * CORRECTED FOR CITIZENS, RE: MIDSTREAM PURCHASE AND ALL
      * EARNINGS ARE PUT INTO DISCOUNT.

           ADD LN-ANTICERN(1) LN-ANTICERN(3) LN-ANTICERN(4)
                                            GIVING AERN-WK-FINCHG.
           IF AERN-WK-FINCHG = 0
              COMPUTE AERN-WK-FINCHG =
                  LN-INTCHG + LN-EXTCHG + LN-SERCHG
                                         + (LN-MAINTFEE * LN-ORGTERM)
           ELSE
              ADD LN-ANTICADJ(1) LN-ANTICADJ(3) LN-ANTICADJ(4)
                                                   TO AERN-WK-FINCHG
              SUBTRACT LN-UFERN(1) LN-UFERN(3) LN-UFERN(4)
                                                  FROM AERN-WK-FINCHG.

           MOVE AERN-WK-FINCHG TO AERN-EARN.
           PERFORM AERN-ACTUARIAL-REBATE-NEW.
           COMPUTE AERN-EARN ROUNDED =
                (AERN-EARN - REB-REBATE)
                    * (AERN-LN-ANTICERN + AERN-LN-ANTICADJ)
                    / AERN-EARN.

      *************************************************
      *                   (53)
      * COMPUTE EARNINGS PRO-RATA OF ORIGINAL TERM:
      *************************************************
       AERN-PRORATA SECTION.
      * SAVE FOR RESTORE OF ORIGINAL VALUE OF AERN-LN-ANTICTERM:
           MOVE AERN-LN-ANTICTERM TO AERN-ANTICTERMW.

           PERFORM AERN-GET-REM-TERM.

           IF (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1)
              IF SP-ERN-TERM(AERN) NOT = 0
                 COMPUTE AERN-REMTERM =  SP-ERN-TERM(AERN) -
                                  (AERN-LN-ANTICTERM - AERN-REMTERM)
                 MOVE SP-ERN-TERM(AERN) TO AERN-LN-ANTICTERM.

           IF AERN-REMTERM < 0
              MOVE 0 TO AERN-EARN.

           IF AERN-REMTERM > 0
              COMPUTE AERN-EARN ROUNDED =
                 ((AERN-LN-ANTICERN - AERN-LN-UFERN)
                             / AERN-LN-ANTICTERM) * AERN-REMTERM.

           IF AERN-REMTERM = 0
              MOVE AERN-LN-ANTICERN TO AERN-EARN
           ELSE
              COMPUTE AERN-EARN = AERN-LN-ANTICERN - AERN-EARN.

       AERN-PRORATA-EXIT.
      * RESTORE ORIGINAL VALUE OF AERN-LN-ANTICTERM:
           MOVE AERN-ANTICTERMW TO AERN-LN-ANTICTERM.

      ****************************************************
      *                 (54)
      *    COMPUTE EARNINGS RULE78-5 METHOD:
      *
      ****************************************************
       AERN-RULE78-5 SECTION.
      * GET ANTICIPATED EARNINGS AMOUNT TO BE SPREAD:
           COMPUTE AERN-ANTICIPATED = AERN-LN-ANTICERN
                                              - AERN-LN-UFERN.

      * DETERMINE THE REMAINING MONTHS LEFT TO SPREAD:
           MOVE AERN-ENDDATE TO NDTE-DATE.
           MOVE 31 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO SYS-DATE.

           IF AERN-LN-SALEFIN NOT = "Y"
              IF SP-ERNACCRL-START-CD(AERN) = " "
                 MOVE 0 TO NDTE-HOLD
                 MOVE AERN-LN-INTDATE TO NDTE-DATE
              ELSE
                 MOVE -1 TO NDTE-HOLD
                 MOVE AERN-LN-ORIG-1STPYDATE TO NDTE-DATE
           ELSE
              IF SP-ERNACCRL-START-CD(AERN) = " "
                 MOVE 0 TO NDTE-HOLD
                 MOVE AERN-LN-PURDATE TO NDTE-DATE
              ELSE
                 IF SP-ERNACCRL-START-CD(AERN) = "A"
                    MOVE 0 TO NDTE-HOLD
                    MOVE AERN-LN-INTDATE TO NDTE-DATE
                 ELSE
                    MOVE -1 TO NDTE-HOLD
                    MOVE AERN-LN-ORIG-1STPYDATE TO NDTE-DATE.
           MOVE 31 TO NDTE-DD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO NUM-DATE.

           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      * IF THE ACCOUNT HAS DEFERMENTS ADVANCE ANTICITERM:
           ADD AERN-LN-ANTICTERM AERN-LN-TOTNODEF GIVING AERN-ANTICTERM.

      * COMPUTE REMAINING TERM WHICH IS ALWAYS 1 LESS:
           COMPUTE AERN-REMTERM = AERN-ANTICTERM - (ELAPSED-MONTHS + 1).

      * IF WITHIN ANTICIPATED TERM COMPUTE REBATE:
           IF AERN-ANTICTERM > 0
              IF (AERN-REMTERM > 0 AND < AERN-ANTICTERM)
                 COMPUTE AERN-EARN ROUNDED = AERN-LN-UFERN +
                   (1 - (((AERN-REMTERM + .5) * (AERN-REMTERM + 1.5))
                   / (AERN-ANTICTERM * (AERN-ANTICTERM + 1))))
                   * AERN-ANTICIPATED.

      * IF LAST MONTH TO TAKE EARNINGS ON, TAKE WHAT IS LEFT:
           IF AERN-REMTERM NOT > 0
              COMPUTE AERN-EARN = AERN-LN-ANTICERN.

      * IF BEFORE START DATE SET EARNINGS TO UPFRONT:
           IF AERN-REMTERM > AERN-ANTICTERM
              COMPUTE AERN-EARN = AERN-LN-UFERN .

       AERN-RULE78-5-EXIT.
           EXIT.

      ****************************************************
      *                 (56)
      *    COMPUTE EARNINGS RULE78 ACCRUAL (CASH METHOD 7)
      *             AS OF THE LAST PAYMENT DATE
      *
      ****************************************************
       AERN-RULE78-CASH SECTION.
           MOVE 0 TO AERN-WORKER.
           SUBTRACT AERN-LN-UFERN FROM AERN-LN-ANTICERN
                                    GIVING AERN-ANTICIPATED.
           MOVE AERN-LN-UFERN TO AERN-EARN.

           MOVE AERN-ENDDATE TO AERN-SAVE-ENDDATE.
           IF LN-DATE-PAID-LAST = 0
              MOVE AERN-LN-PURDATE TO AERN-ENDDATE
           ELSE
              MOVE LN-DATE-PAID-LAST TO AERN-ENDDATE.
           PERFORM AERN-GET-REM-TERM.
           MOVE AERN-SAVE-ENDDATE TO AERN-ENDDATE.

           IF AERN-REMTERM NOT < 0
              IF AERN-REMTERM NOT > AERN-LN-ANTICTERM
                 COMPUTE AERN-EARN ROUNDED = AERN-EARN +
                    (1 - (AERN-REMTERM * (AERN-REMTERM + 1))
                    / ((AERN-LN-ANTICTERM) *
                      ((AERN-LN-ANTICTERM) + 1)))
                    * AERN-ANTICIPATED.

      * IF PAST LAST MONTH TO TAKE EARNINGS ON, TAKE WHAT IS LEFT.
           IF AERN-REMTERM < 0
              MOVE AERN-LN-ANTICERN TO AERN-EARN.

       AERN-RULE78-CASH-EXIT.
           EXIT.

      *******************************************************
      *                 (57)   (GULF ACCEPTANCE FL)
      *
      *   ACTUARIAL PRINICPAL REDUCTION, BASED ON A
      *       THE DIFFERENCE BETWEEN EARNINGS FOR THE
      *       YIELD (INTEREST AND DEALER DISCOUNT)
      *       AND EARNINGS FOR INTEREST
      *******************************************************
       AERN-ACTUARIAL-57 SECTION.
           MOVE LN-APRATE TO AERN-APRATE.
           MOVE AERN-LN-ANTICERN TO AERN-ANTICIPATED.
           MOVE LN-LNAMT TO AERN-LNAMT.
           MOVE LN-REGPYAMT TO AERN-REGPYAMT.
           MOVE LN-1STPYAMT TO AERN-1STPYAMT.
           MOVE LN-LASTPYAMT TO AERN-LASTPYAMT.
           MOVE SP-YEARTYPE TO AERN-SV-YEARTYPE.
           MOVE SP-REBTBL(7) TO AERN-SV-REBTBL.
           MOVE SP-DISFRMLA TO AERN-SV-DISFRMLA.
           MOVE "A" TO SP-DISFRMLA.

           PERFORM AERN-ACTUARIAL-57-63-USE-APR.
           COMPUTE AERN-EARN-APR = LN-INTCHG - REB-REBATE.

           PERFORM AERN-ACTUARIAL-57-USE-YIELD.
           COMPUTE AERN-EARN =
               AERN-LN-ANTICERN + LN-INTCHG - REB-REBATE
                                                - AERN-EARN-APR.

           MOVE AERN-APRATE TO LN-APRATE.
           MOVE AERN-LNAMT TO LN-LNAMT.
           MOVE AERN-REGPYAMT TO LN-REGPYAMT.
           MOVE AERN-1STPYAMT TO LN-1STPYAMT.
           MOVE AERN-LASTPYAMT TO LN-LASTPYAMT.
           MOVE AERN-SV-YEARTYPE TO SP-YEARTYPE.
           MOVE AERN-SV-REBTBL TO SP-REBTBL(7).
           MOVE AERN-SV-DISFRMLA TO SP-DISFRMLA.
           MOVE AERN-SV-INTDATE TO AERN-LN-INTDATE.
           MOVE AERN-SV-LOANDATE TO AERN-LN-LOANDATE.
           MOVE AERN-SV-1STPYDATE TO AERN-LN-ORIG-1STPYDATE.
           MOVE AERN-SV-ORGTERM TO LN-ORGTERM.
           MOVE AERN-ANTICTERM TO AERN-LN-ANTICTERM.

       AERN-ACTUARIAL-57-EXIT.
           EXIT.

       AERN-ACTUARIAL-57-63-USE-APR SECTION.
           PERFORM LOAN-CALCULATIONS.
           MOVE FINANCED-AMOUNT TO APR-FINANCED.
           MOVE LN-INTDATE TO APR-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO APR-1STPYDATE.
           MOVE LN-ORGTERM TO APR-ORGTERM.
           MOVE LN-1STPYAMT TO APR-1STPYAMT.
           MOVE LN-REGPYAMT TO APR-REGPYAMT.
           MOVE LN-LASTPYAMT TO APR-LASTPYAMT.
           MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.
           MOVE "A" TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE APR-APR TO LN-APRATE.

      *    SUBTRACT AERN-LN-UFERN FROM AERN-LN-ANTICERN
      *                             GIVING REB-TOTCHG.
           MOVE LN-INTCHG TO REB-TOTCHG.
           PERFORM AERN-ACTUARIAL-REBATE-OLD.

       AERN-ACTUARIAL-57-USE-YIELD SECTION.
           PERFORM LOAN-CALCULATIONS.
           SUBTRACT LN-DLDISC FROM FINANCED-AMOUNT
                                     GIVING APR-FINANCED.
           MOVE LN-INTDATE TO APR-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO APR-1STPYDATE.
           MOVE LN-ORGTERM TO APR-ORGTERM.
           MOVE LN-1STPYAMT TO APR-1STPYAMT.
           MOVE LN-REGPYAMT TO APR-REGPYAMT.
           MOVE LN-LASTPYAMT TO APR-LASTPYAMT.
           MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.
           MOVE "A" TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE APR-APR TO LN-APRATE.

      *    SUBTRACT AERN-LN-UFERN FROM AERN-LN-ANTICERN
      *                             GIVING REB-TOTCHG.
           ADD LN-ANTICERN(5) LN-ANTICERN(1) GIVING REB-TOTCHG.
           PERFORM AERN-ACTUARIAL-REBATE-OLD.

      ***********************************************************
      *                         (58)
      *     COMPUTE EARNINGS RULE78 METHOD, (FIDELITY / NATIONAL)
      *
      *               LESSOR OF ELAPSED MONTHS
      *                          OR
      *                     PAYMENTS MADE
      ***********************************************************
       AERN-LESSER-78 SECTION.
           COMPUTE AERN-ANTICIPATED = AERN-LN-ANTICERN
                                           - AERN-LN-UFERN.

      * CALCULATE PAYMENTS DUE:
           MOVE AERN-LN-ORIG-1STPYDATE TO NUM-DATE.
           MOVE AERN-ENDDATE TO SYS-DATE.
           PERFORM TIM360.
           COMPUTE AERN-PAYSDUE = ELAPSED-MONTHS + 1.

      * CALCULATE PAYMENTS MADE:
      * - NORMAL PURCHASE:
           IF (LN-ORIGPURAMT = LN-LNAMT) OR (AERN-LN-SALEFIN NOT = "Y")
              COMPUTE AERN-PAYSMADE =
                        (LN-LNAMT - LN-EXTCHG - LN-CURBAL)
                                        / LN-REGPYAMT + 0.34
           ELSE
      * - MIDSTREAM PURCHASE:
              COMPUTE AERN-PAYSMADE =
                        LN-ORGTERM - LN-CURBAL / LN-REGPYAMT.

      * CALCULATE MONTHS REMAINING:

           IF AERN-PAYSDUE < AERN-PAYSMADE
              COMPUTE AERN-REMTERM = LN-ORGTERM - AERN-PAYSDUE
           ELSE
              COMPUTE AERN-REMTERM = LN-ORGTERM - AERN-PAYSMADE.

      * CALCULATE UNEARNED:
           IF AERN-REMTERM > 0
              COMPUTE AERN-EARN ROUNDED = AERN-LN-UFERN +
                      (1 - ((AERN-REMTERM * (AERN-REMTERM + 1))
                      / (LN-ORGTERM * (LN-ORGTERM + 1))))
                      * AERN-ANTICIPATED.


      * IF LAST MONTH TO TAKE EARNINGS ON, TAKE WHAT IS LEFT:
           IF AERN-REMTERM NOT > 0
              COMPUTE AERN-EARN = AERN-LN-ANTICERN.

      * IF BEFORE START DATE SET EARNINGS TO UPFRONT:
           IF AERN-REMTERM > LN-ORGTERM
             COMPUTE AERN-EARN = AERN-LN-UFERN .

       AERN-LESSER-78-EXIT.
           EXIT.

      ***********************************************************
      *                         (59)
      *     COMPUTE EARNINGS REBATE METHOD, (IDEAL)
      ***********************************************************
       AERN-REBATE SECTION.
           IF AERN = 1
              MOVE 7 TO REB-SUB
           ELSE
              IF AERN = 3
                 IF SP-RBFRMLA(8) = "H" OR "I"
                    PERFORM AERN-REBATE-SC-H-OR-I
                    MOVE 8 TO REB-SUB
                 ELSE
                    MOVE 8 TO REB-SUB
              ELSE
                 IF AERN = 4
                    MOVE 9 TO REB-SUB
                 ELSE
                    GO TO AERN-REBATE-EXIT.

           MOVE AERN-LN-ANTICERN TO REB-TOTCHG.
           MOVE AERN-LN-ANTICTERM TO REB-ORGTERM.
           MOVE "PY" TO REB-LPTRCD.
           MOVE AERN-ENDDATE TO REB-PAYDATE.
           PERFORM REBATE.
           IF REB-SUB = 0
              GO TO AERN-REBATE-EXIT.
           IF AERN NOT = 1
              COMPUTE AERN-EARN = AERN-LN-ANTICERN - REB-REBATE
              GO TO AERN-REBATE-EXIT.

           MOVE REB-REBATE TO EADJ-REBATE.
           PERFORM EADJ-EARN-ADJUSTMENT.
           IF EADJ-ADJUST-FG(1) = "Y"
              COMPUTE AERN-EARN = LN-ANTICERN(1)
                                   - EADJ-ADJUST(1).
           IF EADJ-ADJUST-FG(2) = "Y"
              COMPUTE AERN-EARNINGS(2) = LN-ANTICERN(2)
                                   - EADJ-ADJUST(2).
           IF EADJ-ADJUST-FG(3) = "Y"
              COMPUTE AERN-EARNINGS(3) = LN-ANTICERN(3)
                                   - EADJ-ADJUST(3).
           IF EADJ-ADJUST-FG(4) = "Y"
              COMPUTE AERN-EARNINGS(4) = LN-ANTICERN(4)
                                   - EADJ-ADJUST(4).

       AERN-REBATE-EXIT.
           EXIT.

       AERN-REBATE-SC-H-OR-I SECTION.
           IF NOT (LN-INT-REBATE = " " OR "C")
              MOVE LN-ANTICADJ(1) TO REB-STATEWK
           ELSE
              ADD LN-INTCHG LN-EXTCHG GIVING REB-TOTCHG
              MOVE LN-ORGTERM TO REB-ORGTERM
              MOVE "PY" TO REB-LPTRCD
              MOVE AERN-ENDDATE TO REB-PAYDATE
              MOVE 7 TO REB-SUB
              PERFORM REBATE
              IF REB-SUB NOT = 0
                 MOVE REB-REBATE TO REB-STATEWK.

      ****************************************************
      *                (60)
      *    COMPUTE EARNINGS ACTUARIAL METHOD (MERCURY)
      *
      *    60 - CALCULATE NEW APR'S FOR COMPONENTS
      ****************************************************
       AERN-ACTUARIAL-60 SECTION.
           PERFORM AERN-SAVE-LN-SP.
           MOVE "A" TO SP-DISFRMLA.

      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
           PERFORM AERN-SET-WORKERS-NEW.

           IF (SP-ERNACCRL-FG(AERN) = "D") AND
              (LN-CASHDATE NOT = 0       )
              MOVE LN-CASHDATE  TO NUM-DATE
              MOVE AERN-ENDDATE TO SYS-DATE
              IF SYS-DATE < NUM-DATE
                 MOVE 0 TO AERN-EARN
                 GO TO AERN-ACTUARIAL-60-EXIT.

           IF (AERN-LN-SALEFIN = "Y")
                  AND (LN-ORIGPURAMT NOT = LN-LNAMT)
              PERFORM AERN-MIDSTREAM-60
              PERFORM AERN-GET-APR-COMPONENT-60
              PERFORM AERN-ACTUARIAL-COMPONENT-60
           ELSE
           IF ( AERN = 1           AND
                LN-ANTICERN(2) = 0 AND
                LN-ANTICERN(3) = 0 AND
                LN-ANTICERN(4) = 0 AND
                (LN-DLDISC = 0 OR LN-ANTICERN(5) NOT = 0) AND
                LN-UFERN(1)    = 0 AND
                AERN-LN-SALEFIN = "N"
              )
              PERFORM AERN-ACTUARIAL-COMPONENT-60
           ELSE
              PERFORM AERN-GET-APR-COMPONENT-60
              PERFORM AERN-ACTUARIAL-COMPONENT-60.

           PERFORM AERN-RESTORE-LN-SP.

       AERN-ACTUARIAL-60-EXIT.
           EXIT.

      ***************************************************
      *    (60)
      *    GET NEW APR FOR COMPONENT
      ***************************************************
       AERN-GET-APR-COMPONENT-60 SECTION.
           COMPUTE AERN-WK-FINANCED =
              AERN-WK-1STPYAMT
                  + (AERN-WK-NOREGPY * AERN-WK-REGPYAMT)
                   + AERN-WK-LASTPYAMT
                    + AERN-WK-UFERN
                     - (AERN-WK-ANTICERN + AERN-WK-ANTICADJ).

           MOVE " " TO AERN-WK-INCLUDE-DEF-FG.

           MOVE AERN-WK-1STPYAMT TO APR-1STPYAMT.
           MOVE AERN-WK-REGPYAMT TO APR-REGPYAMT.
           MOVE AERN-WK-LASTPYAMT TO APR-LASTPYAMT.
           MOVE AERN-WK-ORGTERM TO APR-ORGTERM.
           MOVE AERN-WK-INTDATE TO APR-INTDATE.
           MOVE AERN-WK-ORIG-1STPYDATE TO APR-1STPYDATE.
           MOVE AERN-WK-FINANCED TO APR-FINANCED.
           MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.

           MOVE AERN-WK-DISFRMLA TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE APR-APR TO AERN-WK-APRATE.

      *****************************************************
      *    (60)
      *    ACTUARIAL COMPONENT ROUTINE
      *****************************************************
       AERN-ACTUARIAL-COMPONENT-60 SECTION.
           SUBTRACT AERN-LN-UFERN FROM AERN-LN-ANTICERN
                                    GIVING AERN-WK-FINCHG.
           PERFORM AERN-ACTUARIAL-REBATE-NEW.
           COMPUTE AERN-EARN = AERN-LN-ANTICERN - REB-REBATE.


      ***************************************************
      *    (60)
      *    SETUP FOR MIDSTREAM PAPER ACTUARIAL CALC
      ***************************************************
       AERN-MIDSTREAM-60 SECTION.
           MOVE AERN-WK-PURDATE TO AERN-WK-INTDATE
                                   AERN-WK-LOANDATE.

      * DETERMINE ELAPSED TIME REMAINING BETWEEN
      * PURCHASE DATE AND CURRECT MATURITY EXCLUDING DEFERMENTS:
           MOVE "Y" TO AERN-MDTE-FLAG
                       AERN-MDTE-ORGTERM-FG.
           PERFORM AERN-MATURITY-DATE-60.

           MOVE AERN-WK-PURDATE TO NUM-DATE.
           MOVE AERN-MDTE-DATE TO SYS-DATE.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.

      * SET FRACTIONAL PERIOD FROM PAYDATE TO NEXT PAYMENT DUE DATE:
           MOVE ELAPSED-UNITPER-REM TO AERN-PPAYS.

      * SET WHOLE PERIOD(S) FROM PURCHASE DATE TO MATURITY:
           MOVE ELAPSED-UNITPER TO AERN-WPAYS.
           IF ELAPSED-UNITPER-REM NOT = 0
              ADD 1 TO AERN-WPAYS.

           IF AERN-WPAYS < AERN-WK-ORGTERM
              MOVE AERN-WPAYS TO AERN-WK-ANTICTERM
                                 AERN-WK-ORGTERM
              MOVE AERN-WK-REGPYAMT TO AERN-WK-1STPYAMT
              SUBTRACT 1 FROM AERN-WK-ORGTERM
                                        GIVING AERN-WK-NOREGPY
              IF AERN-WK-LASTPYAMT NOT = 0
                 SUBTRACT 1 FROM AERN-WK-NOREGPY
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE AERN-WK-ORGTERM TO AERN-WK-ANTICTERM.

      *    MOVE AERN-WK-ORIG-1STPYDATE TO NUM-DATE.
           MOVE AERN-WK-1STPYDATE TO NUM-DATE.
           MOVE AERN-WK-PURDATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER NOT < 0
              PERFORM AERN-MIDSTREAM-60-PUR-AFTER.

      * ADJUST 1ST PAYMENT FOR LOAN DELINQUENCY OR PAID AHEAD
      * AT MIDSTREAM PURCHASE TIME:
           COMPUTE AERN-WORKER =
               AERN-WK-ORIGPURAMT
                  - ( AERN-WK-NOREGPY * AERN-WK-REGPYAMT
                                         + AERN-WK-LASTPYAMT ).
           IF AERN-WORKER > 0
              MOVE AERN-WORKER TO AERN-WK-1STPYAMT
              MOVE AERN-WK-1STPYDATE TO AERN-WK-ORIG-1STPYDATE
              GO TO AERN-MIDSTREAM-60-EXIT.

      *---------------------------------------------------------
      *  EXAMPLE:
      *                       TERM = 1     TERM = 2     TERM = 6
      *                     -----------  -----------  -----------
      *   LN-1STPYAMT    :  $100.00      $100.00      $100.00
      *   LN-REGPYAMT    :  $100.00 X 0  $120.00 X 0  $120.00 X 4
      *   LN-LASTPYAMT   :  $  0.00      $120.00      $130.00
      *                      ------       ------       ------
      *      TOTAL REPAY :  $100.00      $220.00      $710.00
      *---------------------------------------------------------
           MOVE 0 TO AERN-WK-ANTICTERM
                     AERN-WK-ORGTERM.
           MOVE AERN-WK-ORIGPURAMT TO AERN-WORKER.
           IF AERN-WORKER > AERN-WK-LASTPYAMT
              ADD 1 TO AERN-WK-ANTICTERM
                       AERN-WK-ORGTERM
              SUBTRACT AERN-WK-LASTPYAMT FROM AERN-WORKER
           ELSE
      * SETUP FOR SINGLE PAY LOAN:
              MOVE AERN-WORKER TO AERN-WK-1STPYAMT
                                  AERN-WK-REGPYAMT
              MOVE 0 TO AERN-WK-NOREGPY
                        AERN-WK-LASTPYAMT
              MOVE 1 TO AERN-WK-ANTICTERM
                        AERN-WK-ORGTERM
              GO TO AERN-MIDSTREAM-60-1ST.
           DIVIDE AERN-WK-REGPYAMT INTO AERN-WORKER
                                         GIVING AERN-WORKNO.
           IF AERN-WORKNO = 0
      * SETUP FOR TWO PAY LOAN:
              MOVE AERN-WORKER TO AERN-WK-1STPYAMT
              MOVE AERN-WK-LASTPYAMT TO AERN-WK-REGPYAMT
              MOVE 0 TO AERN-WK-NOREGPY
              MOVE 2 TO AERN-WK-ANTICTERM
                        AERN-WK-ORGTERM
              GO TO AERN-MIDSTREAM-60-1ST.

           MOVE AERN-WORKNO TO AERN-WK-NOREGPY.
           COMPUTE AERN-WORKER =
                 AERN-WORKER - AERN-WORKNO * AERN-WK-REGPYAMT.
           ADD AERN-WORKNO TO AERN-WK-ANTICTERM
                              AERN-WK-ORGTERM.
           IF AERN-WORKER = 0
      * 1STPYAMT = REGPYAMT:
              MOVE AERN-WK-REGPYAMT TO AERN-WK-1STPYAMT
              SUBTRACT 1 FROM AERN-WK-NOREGPY
              GO TO AERN-MIDSTREAM-60-1ST.

      * FIRST PAYMENT:
           MOVE AERN-WORKER TO AERN-WK-1STPYAMT.
           ADD 1 TO AERN-WK-ANTICTERM
                    AERN-WK-ORGTERM.

        AERN-MIDSTREAM-60-1ST.
           MOVE AERN-MDTE-DATE TO NDTE-DATE.
           ADD 1 AERN-WK-NOREGPY GIVING NDTE-HOLD.
           IF AERN-WK-LASTPYAMT = 0
              SUBTRACT 1 FROM NDTE-HOLD.
           MULTIPLY -1 BY NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO AERN-WK-ORIG-1STPYDATE.

       AERN-MIDSTREAM-60-EXIT.
           EXIT.

       AERN-MIDSTREAM-60-PUR-AFTER SECTION.
      *    MOVE AERN-WK-ORIG-1STPYDATE TO NDTE-DATE.
           MOVE AERN-WK-1STPYDATE TO NDTE-DATE.
           MOVE ELAPSED-UNITPER TO NDTE-HOLD.
           IF ELAPSED-UNITPER-REM NOT < 0
              ADD 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO AERN-WK-1STPYDATE.

       AERN-MATURITY-DATE-60 SECTION.
           MOVE AERN-WK-1STPYDATE TO NDTE-DATE.
           MOVE 0 TO NDTE-HOLD.
           IF AERN-MDTE-FLAG = " "
              MOVE AERN-WK-TOTNODEF TO NDTE-HOLD.

           IF AERN-MDTE-ORGTERM-FG = "Y"
              COMPUTE NDTE-HOLD = NDTE-HOLD + (AERN-WK-ORGTERM - 1)
           ELSE
              COMPUTE NDTE-HOLD = NDTE-HOLD + (AERN-WK-SUM-ORGTERM - 1).

           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO AERN-MDTE-DATE.
           MOVE " " TO AERN-MDTE-FLAG
                       AERN-MDTE-ORGTERM-FG.

      *******************************************************
      *                 (62)
      *    COMPUTE EARNINGS ACTUARIAL, PRORATING COMPONENT
      *    TO ACTUARIAL EARNINGS (YIELD) OF ALL COMPONENTS,
      *    INTEREST, SERVICE CHARGE, MAINTENANCE FEES
      *    AND DEALER DISCOUNT
      *
      *******************************************************
       AERN-ACTUARIAL-62 SECTION.
           PERFORM AERN-SAVE-LN-SP.
           MOVE "A" TO SP-DISFRMLA.

      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
           PERFORM AERN-SET-WORKERS-NEW.
           IF AERN-LN-SALEFIN = "Y"
              MOVE AERN-WK-PURDATE        TO NUM-DATE
              MOVE AERN-WK-ORIG-1STPYDATE TO SYS-DATE
              IF SYS-DATE < NUM-DATE
                       OR (LN-ORIGPURAMT NOT = LN-LNAMT)
                 PERFORM AERN-MIDSTREAM-NEW.

           PERFORM AERN-APR-COMPONENT-NEW.
           PERFORM AERN-ACTUARIAL-REGZ-NEW.

      * GET TOTAL ANTICIPATED ALL COMPONENTS:
           COMPUTE AERN-WORKER =
                 LN-ANTICERN(1) + LN-ANTICADJ(1)
                 + LN-ANTICERN(3) + LN-ANTICADJ(3)
                  + LN-ANTICERN(4) + LN-ANTICADJ(4)
                   + LN-ANTICERN(5) + LN-ANTICADJ(5)
                    + LN-ANTICERN(6) + LN-ANTICADJ(6).

      * GET COMPONENT UNEARNED PRORATA:
           COMPUTE AERN-WORKER ROUNDED =
               REGZ-UNEARNED
                  * (AERN-WK-ANTICERN + AERN-WK-ANTICADJ)
                      / AERN-WORKER.

           COMPUTE AERN-EARN = LN-ANTICERN(AERN)
                         - AERN-WORKER - LN-ACCERN(AERN).

           PERFORM AERN-RESTORE-LN-SP.

       AERN-ACTUARIAL-62-EXIT.
           EXIT.

      *************************************************************
      *                 (63)
      *            FOR INTEREST AND/OR DISCOUNT ONLY
      *
      *    INTEREST - COMPUTE EARNINGS ACTUARIAL BASED ON APR
      *    DISCOUNT - COMPUTE EARNINGS ACTUARIAL USING (YIELD)
      *               OF INTEREST AND DEALER DISCOUNT, THAN
      *               REDUCE YIELD EARNINGS BY INTEREST EARNINGS
      *               PER ABOVE
      *
      *************************************************************
       AERN-ACTUARIAL-63 SECTION.
           PERFORM AERN-SAVE-LN-SP.
           MOVE "A" TO SP-DISFRMLA.

      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
           PERFORM AERN-SET-WORKERS-NEW.

           IF (AERN = 1)
                     OR (AERN = 5 AND AERN-63-INT-EARN = 0)
              PERFORM AERN-ACTUARIAL-57-63-USE-APR
              COMPUTE AERN-EARN
                      AERN-63-INT-EARN =
                            LN-ANTICERN(1) - REB-REBATE
              COMPUTE AERN-EARN = AERN-EARN - LN-ACCERN(1).

           IF AERN = 5
      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
              IF AERN-LN-SALEFIN = "Y"
                 MOVE AERN-WK-PURDATE        TO NUM-DATE
                 MOVE AERN-WK-ORIG-1STPYDATE TO SYS-DATE
                 IF SYS-DATE < NUM-DATE
                          OR (LN-ORIGPURAMT NOT = LN-LNAMT)
                    PERFORM AERN-MIDSTREAM-NEW
                 END-IF
              END-IF
      * GET YIELD RATE FOR INTEREST AND DISCOUNT CHARGES:
              PERFORM AERN-APR-COMPONENT-NEW
      * GET UNEARNED VIA YIELD:
              PERFORM AERN-ACTUARIAL-REGZ-NEW
      * GET TOTAL ANTICIPATED INT & DISC COMPONENTS:
              COMPUTE AERN-WORKER =
                    LN-ANTICERN(1) + LN-ANTICADJ(1)
                      + LN-ANTICERN(5) + LN-ANTICADJ(5)
      * GET DISCOUNT UNEARNED:
              COMPUTE AERN-EARN =
                    AERN-WORKER - REGZ-UNEARNED - AERN-63-INT-EARN
      * GET DISCOUNT EARNINGS:
              COMPUTE AERN-EARN = AERN-EARN - LN-ACCERN(5)
      * INSURE EARNINGS DON'T EXCEED ANTICIPATED AND UNEARNED:
              IF AERN-EARN >
                    ( LN-ANTICERN(5) + LN-ANTICADJ(5) - LN-ACCERN(5) )
                 COMPUTE AERN-EARN =
                    ( LN-ANTICERN(5) + LN-ANTICADJ(5) - LN-ACCERN(5) )
              END-IF
              IF AERN-63-INT-EARN NOT < 0
                 IF AERN-EARN < 0
                    MOVE 0 TO AERN-EARN
                 END-IF
              END-IF
              MOVE 0 TO AERN-63-INT-EARN.

           PERFORM AERN-RESTORE-LN-SP.

       AERN-ACTUARIAL-63-EXIT.
           EXIT.

      ********************************************************************
      *                (64   MERCURY)
      *
      *     SAME RATIO OF EARNINGS AS INTEREST IS EARNED VIA: ERNFRMLA 60
      *
      *           ...WHEN THE INTEREST CHARGE IS ZERO(0) OR ACCOUNT IS
      *              'IB' MIDSTREAM, EARNINGS WILL BE COMPUTED VIA
      *              SP-ERNFRMLA 51 (RULE 78THS)
      *
      *           ...WHEN ACCOUNT IS 'IB' ACCOUNT IS VIEWED AS IF IT
      *              WAS 'PC' AND EARNINGS FOR PERIOD IS DETERMINED
      *              VIA ERNFRMLA = 60
      *
      ********************************************************************
       AERN-64-VIA60 SECTION.

      *       ------------------------------------------------------------
      *       IF INTEREST CHARGE IS ZERO OR ACCOUNT IS 'IB'
      *          MIDSTREAM PURCHASE WILL COMPUTE EARNINGS
      *          PER SP-ERNFRMLA 51, AERN-RULE78
      *       ------------------------------------------------------------
           IF ( LN-INTCHG = 0 ) OR
                      ( ( LN-LOANTYPE = "I" ) AND
                        ( LN-ORIGPURAMT NOT = LN-LNAMT )
                      )
      *       PERFORM AERN-RULE78
              PERFORM AERN-RULE78-LOGIC
           ELSE
           IF LN-LOANTYPE = "P"
      *    ------------------------------------------------------------
      *    COMPUTE PRORATED NEW UNEARNED:
      *    -----------------------------------------------------------
              COMPUTE AERN-WORKER ROUNDED =
                  (
                    LN-ANTICERN(1) - (LN-ACCERN(1) + AERN-EARNINGS(1))
                  )
                    * (AERN-LN-ANTICERN - AERN-LN-UFERN)
                                           / LN-ANTICERN(1)

      *      COMPUTE EARNINGS AS DIFFERENCE BETWEEN ANTIC & NEW UNEARNED:
              COMPUTE AERN-EARN ROUNDED =
                  AERN-LN-ANTICERN - AERN-WORKER
           ELSE
      *-------------------------------------------------------------------
      *       FOR LOAN TYPE 'IB' AND IF SO GET EARNINGS FOR INTEREST
      *       AS IF THE ACCOUNT WAS 'PC' PAPER:
      *-------------------------------------------------------------------
              MOVE AERN TO AERN-HOLD
              MOVE 1 TO AERN

      *      MUST ESTABLISH AERN-LN-ANTIC<>FIELDS FOR IB INTEREST:
              MOVE AERN-LN-ANTICTERM   TO AERN-HOLD-ANTICTERM
              MOVE AERN-LN-ANTICERN    TO AERN-HOLD-ANTICERN
              MOVE AERN-LN-ANTICADJ    TO AERN-HOLD-ANTICADJ
              MOVE AERN-LN-UFERN       TO AERN-HOLD-UFERN
              MOVE AERN-LN-ACCERN      TO AERN-HOLD-ACCERN

              MOVE LN-INTCHG           TO AERN-LN-ANTICERN
              MOVE 0                   TO AERN-LN-ANTICADJ
              MOVE 0                   TO AERN-LN-UFERN
              MOVE 0                   TO AERN-LN-ACCERN
              PERFORM AERN-ACTUARIAL-60

              MOVE AERN-HOLD-ANTICTERM TO AERN-LN-ANTICTERM
              MOVE AERN-HOLD-ANTICERN  TO AERN-LN-ANTICERN
              MOVE AERN-HOLD-ANTICADJ  TO AERN-LN-ANTICADJ
              MOVE AERN-HOLD-UFERN     TO AERN-LN-UFERN
              MOVE AERN-HOLD-ACCERN    TO AERN-LN-ACCERN

              MOVE AERN-HOLD           TO AERN

      *      COMPUTE PRORATED NEW UNEARNED:
              COMPUTE AERN-WORKER ROUNDED =
                    (LN-INTCHG - AERN-EARN)
                        * (AERN-LN-ANTICERN - AERN-LN-UFERN)
                             / LN-INTCHG

      *      COMPUTE EARNINGS AS DIFFERENCE BETWEEN ANTIC & NEW UNEARNED:
              COMPUTE AERN-EARN ROUNDED =
                  AERN-LN-ANTICERN - AERN-WORKER.

       AERN-64-VIA60-EXIT.

       AERN-64-VIA60-EXIT.
           EXIT.

      /****************************************************
      *    TEST FOR DELIQUENCY STOP/START
      *****************************************************
       AERN-ERNDELQ-STOP-START-TEST SECTION.
      *----------------------------------------------------------------------
      * (A)
      *    STOP  TAKING EARNINGS WHEN ACCOUNT REACHES THE CONTRACTUAL
      *          AGE OF SP-ERNDELQ-STOP
      *    START TAKING EARNINGS AGAIN WHEN CONTRACTUAL AGE RETURNS
      *          TO SP-ERNDELQ-START
      *----------------------------------------------------------------------
           IF SP-ERNDELQFRMLA = "A"
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TEST IF THE ACCOUNT IN IS THE STATUS OF TAKING INCOME
      *    IF SO, SEE IF IT SHOULD BE STOPPED:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF AERN-ERNDELQ-STOP = " "
                 IF CONTRACTUAL NOT < SP-ERNDELQ-STOP
                    MOVE "Y" TO AERN-ERNDELQ-STOP
                 END-IF
              ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ACCOUNT IS IN THE STATUS OF NOT TAKING INCOME
      *    SEE IF IT SHOULD BEGIN AGAIN:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                 IF CONTRACTUAL NOT > SP-ERNDELQ-START
                    MOVE " " TO AERN-ERNDELQ-STOP.

      *----------------------------------------------------------------------
      * (B)
      *    STOP  TAKING EARNINGS WHEN ACCOUNT REACHES CONTRACTUAL
      *          AGE OF SP-ERNDELQ-STOP
      *    NEVER START TAKING EARNINGS AGAIN. ACCOUNT IS NOW USING
      *          CASH METHOD
      *----------------------------------------------------------------------
            IF SP-ERNDELQFRMLA = "B"
               IF AERN-ERNDELQ-STOP = " "
                  IF CONTRACTUAL NOT < SP-ERNDELQ-STOP
                     MOVE "Y" TO AERN-ERNDELQ-STOP.

      *----------------------------------------------------------------------
      * (C)
      *    STOP  TAKING EARNINGS WHEN ACCOUNT REACHES THE CONTRACTUAL
      *          AGE OF SP-ERNDELQ-STOP
      *    TAKE  EARNING SLICES FOR EACH CHANGE IN CONTRACTUAL OR
      *          FOR EACH CONTRACTUAL PAYMENT MADE THIS MONTH
      *----------------------------------------------------------------------
           IF SP-ERNDELQFRMLA = "C"
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *       TEST IF THE ACCOUNT IN IS THE STATUS OF TAKING INCOME
      *       IF SO, SEE IF IT SHOULD BE STOPPED:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF AERN-ERNDELQ-STOP = " "
                 IF CONTRACTUAL >= SP-ERNDELQ-STOP
                    MOVE "Y" TO AERN-ERNDELQ-STOP
                 END-IF
              ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *          DETERMINE IF AN ACCRUAL SHOULD TAKE PLACE THIS MONTH
      *          AND IF SO THE NUMBER OF SLICES TO TAKE FROM THE LAST
      *          ACCRUAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                 MOVE LN-CONPROFILE(1) TO AERN-PROFILE-WORKER
                 IF ( AERN-PROFILE-WK(1) = 9   ) OR
                    ( LN-LAST-ACCRUAL-DATE = 0 )
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *             THIS SHOULD NEVER HAPPEN SINCE ACCOUNT IS STOPPED
      *             BUT IF IT DOES, SINCE THE ACCOUNT HAS NEVER BEEN
      *             THRU A MONTH END WE CAN'T DETERMINE A CHANGE
      *             IN CONTRACTUAL STATUS......SO TAKE NO EARNINGS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                    NEXT SENTENCE
                 ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *             DETERMINE IF A REDUCTION IN CONTRACTUAL STATUS OCCURED
      *             THIS MONTH AND IF SO DETERMINE MONTH TO ACCRUAL THRU
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                    IF ( AERN-PROFILE-WK(1)  = 7 ) AND
                       ( (CONTRACTUAL / 30) >= 7  )
                       MOVE AERN-ENDDATE      TO NUM-DATE
                       MOVE LN-DATE-PAID-LAST TO SYS-DATE
                       IF ( NUM-MO = S-MM ) AND
                          ( NUM-YR = S-YY )
                          MOVE 0 TO NDTE-HOLD
                       ELSE
                          MOVE -1 TO NDTE-HOLD
                       END-IF
                    ELSE
                       COMPUTE NDTE-HOLD =
                            AERN-PROFILE-WK(1) - CONTRACTUAL / 30
                    END-IF
                    IF NDTE-HOLD >= 0
                       MOVE LN-LAST-ACCRUAL-DATE TO NDTE-DATE
                       ADD 1 TO NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE TO AERN-ENDDATE
                       MOVE "T" TO AERN-ERNDELQ-STOP.


      *SP-ERNDELQFRMLA 'R', RE-ADJUST EARNINGS TO REFLECT CASH EARNINGS
      *AND EARNINGS WILL BE CASH ON A GO FORWARD BASIS

            IF SP-ERNDELQFRMLA = "R"
               IF AERN-ERNDELQ-STOP = " "
                  IF CONTRACTUAL NOT < SP-ERNDELQ-STOP
                     MOVE "R" TO AERN-ERNDELQ-STOP.

      *----------------------------------------------------------------------




      ************************************
      *    COMPUTE ACTUARIAL REBATE  N E W
      ************************************
       AERN-ACTUARIAL-REBATE-NEW SECTION.
           IF SP-ERNACCRL-DFCD(AERN) = "A"
              MOVE AERN-ENDDATE TO NDTE-DATE
              MOVE 40           TO NDTE-DD
              MOVE 0            TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
      *       - - - NEW LOGIC FOR PR#245
      *             TEST FOR ACCRUAL ON LAST DAY OF MONTH
      *             IF SO, ASSURE BACKED UP DATE WILL BE
      *             LAST DAY IN NEW MONTH
              IF NDTE-DATE = AERN-ENDDATE
                 MOVE 40 TO NDTE-DD
              END-IF
      *PR#245 MOVE 40 TO NDTE-DD  ALLOW DAILY ACCRUALS TO WORK
              MULTIPLY -1 BY AERN-LN-TOTNODEF GIVING NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO REB-PAYDATE
              MOVE " " TO REB-SKIPDEF-FG
           ELSE
              MOVE AERN-ENDDATE TO REB-PAYDATE
              MOVE "Y" TO REB-SKIPDEF-FG.

           MOVE AERN-LN-ANTICTERM TO REB-ORGTERM.
           MOVE REB-INTSUB TO REB-SUB.
           MOVE "F" TO SP-RBFRMLA(REB-INTSUB).
           MOVE 0 TO SP-RBDEDCHG(REB-INTSUB).
           MOVE 0 TO SP-RBNOMON(REB-INTSUB).
           MOVE 0 TO SP-RBREDUC(REB-INTSUB).
           MOVE 0 TO SP-RBMINRET(REB-INTSUB).
           MOVE 0 TO SP-RBRECAST(REB-INTSUB).
           MOVE 0 TO SP-RBEARLY(REB-INTSUB).
           MOVE 0 TO SP-RBDEFER(REB-INTSUB).

      * NEW 021098 JTG FOR MERCURY WITH SPECIAL OPTION FORMULAS
           MOVE 0 TO SP-RBSPOPT1(REB-INTSUB)
                     SP-RBSPOPT2(REB-INTSUB).
      * NEW 021098 JTG

           MOVE 100 TO SP-RBPCT(REB-INTSUB).
           MOVE 0 TO SP-RBMIN(REB-INTSUB).
           MOVE 360 TO SP-RBYRTYPE(REB-INTSUB 1)
                       SP-RBYRTYPE(REB-INTSUB 2) SP-YEARTYPE.
           MOVE "B" TO SP-RBDTE(REB-INTSUB 1) SP-RBDTE(REB-INTSUB 2).

      * NEW 021098 JTG RE: TFC #637
      * NEW 070501 JTG RE: REGENCY #1456 'C'
           MOVE " "                     TO REB-EARN-FULL-MONTH.
           IF SP-ERNACCRL-FG(AERN) = "B" OR "C"
              MOVE SP-ERNACCRL-FG(AERN) TO REB-EARN-FULL-MONTH
           ELSE
      * NEW 070501 JTG RE: REGENCY #1456 'C'
      * NEW 021098 JTG RE: TFC #637

           IF SP-ERNACCRL-FG(AERN) = " "
              MOVE 0 TO SP-RBDAYS(REB-INTSUB 1) SP-RBDAYS(REB-INTSUB 2)
           ELSE
              MOVE SP-ERN-DAYS(AERN) TO SP-RBDAYS(REB-INTSUB 1)
                                             SP-RBDAYS(REB-INTSUB 2).

           MOVE AERN-WK-FINCHG         TO REB-TOTCHG.
           MOVE AERN-WK-ORIG-1STPYDATE TO REB-LN-ORIG-1STPYDATE.
           MOVE AERN-WK-LOANDATE       TO REB-LN-LOANDATE.
           MOVE AERN-WK-INTDATE        TO REB-LN-INTDATE.
           MOVE AERN-WK-APRATE         TO REB-LN-APRATE.
           MOVE AERN-WK-TOTNODEF       TO REB-LN-TOTNODEF.

      *    MOVE LN-PURDATE TO REB-LN-PURDATE.
      *    MOVE LN-INSCOVR(2) TO REB-LN-INSCOVR.
      *    MOVE AERN-LN-SALEFIN TO REB-LN-SALEFIN.

           MOVE AERN-WK-ORGTERM      TO REB-LN-ORGTERM.
           MOVE AERN-WK-1STPYAMT     TO REB-LN-1STPYAMT.
           MOVE AERN-WK-REGPYAMT     TO REB-LN-REGPYAMT.
           MOVE AERN-WK-LASTPYAMT    TO REB-LN-LASTPYAMT.
           MOVE "Y"                  TO REB-ADDON-FG.

           PERFORM REBATE.


      ************************************
      *    COMPUTE ACTUARIAL REBATE  O L D
      ************************************
       AERN-ACTUARIAL-REBATE-OLD SECTION.
           IF SP-ERNACCRL-DFCD(AERN) = "A"
              MOVE AERN-ENDDATE TO NDTE-DATE
              MOVE 40           TO NDTE-DD
              MOVE 0            TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
      *       - - - NEW LOGIC FOR PR#245
      *             TEST FOR ACCRUAL ON LAST DAY OF MONTH'
      *             IF SO, ASSURE BACKED UP DATE WILL BE
      *             LAST DAY IN NEW MONTH
              IF NDTE-DATE = AERN-ENDDATE
                 MOVE 40 TO NDTE-DD
              END-IF
      *PR#245 MOVE 40 TO NDTE-DD  ALLOW DAILY ACCRUALS TO WORK
              MULTIPLY -1 BY AERN-LN-TOTNODEF GIVING NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO REB-PAYDATE
              MOVE " " TO REB-SKIPDEF-FG
           ELSE
              MOVE AERN-ENDDATE TO REB-PAYDATE
              MOVE "Y" TO REB-SKIPDEF-FG.
           MOVE AERN-LN-ANTICTERM TO REB-ORGTERM.
           MOVE REB-INTSUB TO REB-SUB.
           MOVE "F" TO SP-RBFRMLA(REB-INTSUB).
           MOVE 0 TO SP-RBDEDCHG(REB-INTSUB).
           MOVE 0 TO SP-RBNOMON(REB-INTSUB).
           MOVE 0 TO SP-RBREDUC(REB-INTSUB).
           MOVE 0 TO SP-RBMINRET(REB-INTSUB).
           MOVE 0 TO SP-RBRECAST(REB-INTSUB).
           MOVE 0 TO SP-RBEARLY(REB-INTSUB).
           MOVE 0 TO SP-RBDEFER(REB-INTSUB).

      * NEW 021098 JTG FOR MERCURY WITH SPECIAL OPTION FORMULAS
           MOVE 0 TO SP-RBSPOPT1(REB-INTSUB)
                     SP-RBSPOPT2(REB-INTSUB).
      * NEW 021098 JTG

           MOVE 100 TO SP-RBPCT(REB-INTSUB).
           MOVE 0 TO SP-RBMIN(REB-INTSUB).
           MOVE 360 TO SP-RBYRTYPE(REB-INTSUB 1)
                       SP-RBYRTYPE(REB-INTSUB 2) SP-YEARTYPE.
           MOVE "B" TO SP-RBDTE(REB-INTSUB 1) SP-RBDTE(REB-INTSUB 2).

      * NEW 021098 JTG RE: TFC #637
      * NEW 070501 JTG RE: REGENCY #1456 'C'
           MOVE " "                     TO REB-EARN-FULL-MONTH.
           IF SP-ERNACCRL-FG(AERN) = "B" OR "C"
              MOVE SP-ERNACCRL-FG(AERN) TO REB-EARN-FULL-MONTH
           ELSE
      * NEW 070501 JTG RE: REGENCY #1456 'C'
      * NEW 021098 JTG RE: TFC #637

           IF SP-ERNACCRL-FG(AERN) = " "
              MOVE 0 TO SP-RBDAYS(REB-INTSUB 1) SP-RBDAYS(REB-INTSUB 2)
           ELSE
              MOVE SP-ERN-DAYS(AERN) TO SP-RBDAYS(REB-INTSUB 1)
                                             SP-RBDAYS(REB-INTSUB 2).

           PERFORM REBATE.

      ***************************************************
      *    GET THE REMAINING TERM
      ***************************************************
       AERN-GET-REM-TERM SECTION.
           MOVE 0 TO AERN-REMTERM.

      * TEST FOR OTHER THAN MONTHLY PAPER:
           IF NOT (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1)
              MOVE AERN-LN-ORIG-1STPYDATE TO CEPP1-DATE
              MOVE AERN-ENDDATE           TO CEPP2-DATE
              PERFORM CONTRACTUAL-ELAPSED-PAYPER
              MOVE CEPP-PERIODS TO AERN-WORK1
              IF AERN-LN-SALEFIN = "Y"
                 MOVE AERN-LN-ORIG-1STPYDATE TO CEPP1-DATE
                 MOVE AERN-LN-PURDATE        TO CEPP2-DATE
                 PERFORM CONTRACTUAL-ELAPSED-PAYPER
                 COMPUTE AERN-REMTERM =
                     AERN-LN-ANTICTERM - (AERN-WORK1 - CEPP-PERIODS)
              END-IF
              IF AERN-REMTERM < 0
                 MOVE 0 TO AERN-REMTERM
              END-IF
              GO TO AERN-GET-REM-TERM-EXIT.


      * SET START DATE (NUM-DATE):
           IF AERN-LN-SALEFIN NOT = "Y"
              IF SP-ERNACCRL-START-CD(AERN) = " "
                 MOVE AERN-LN-INTDATE TO NDTE-DATE
              ELSE
                 MOVE -1 TO NDTE-HOLD
                 MOVE AERN-LN-ORIG-1STPYDATE TO NDTE-DATE
                 PERFORM INCREMENT-MONTHS
           ELSE
              IF SP-ERNACCRL-START-CD(AERN) = " "
                 MOVE AERN-LN-PURDATE TO NDTE-DATE
              ELSE
              IF SP-ERNACCRL-START-CD(AERN) = "A"
                 MOVE AERN-LN-INTDATE TO NDTE-DATE
              ELSE
                 MOVE -1 TO NDTE-HOLD
                 MOVE AERN-LN-ORIG-1STPYDATE TO NDTE-DATE
                 PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AERN-SAVE-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF SP-ERNFRMLA(AERN) NOT = 56
              IF SP-ERNACCRL-FG(AERN) NOT = "C"
                 MOVE 31 TO NDTE-DD
                 PERFORM NEW-DATE-CALCULATION.

      *SEE IF START DATE IS EQUAL TO ACCRUAL DATE:
      *       AERN-FLAG1 = 0  START DATE IS NOT ACCRUAL DATE
      *                  = 1  START DATE = ACCRUAL DATE
           IF NDTE-DATE = AERN-SAVE-DATE
              MOVE 1 TO AERN-FLAG1
           ELSE
              MOVE 0 TO AERN-FLAG1.

           MOVE NDTE-DATE TO NUM-DATE.

           IF SP-ERNFRMLA(AERN) = 56
              MOVE AERN-ENDDATE TO SYS-DATE
           ELSE
      *      FORCE LAST DAY OF MONTH FOR ENDDATE (SYS-DATE):
              MOVE AERN-ENDDATE TO NDTE-DATE
              MOVE 31 TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO SYS-DATE.

           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

           COMPUTE AERN-REMTERM = AERN-LN-ANTICTERM - ELAPSED-MONTHS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      * CHECK SP-ERNACCRL-FG TO DETERMINE WHEN EARNINGS BEGIN:
      * IF " " TAKE FIRST MONTH
      * IF "A" TAKE FIRST MONTH IF START-DATE NOT LAST DAY OF MONTH
      * IF "B" BEGIN ACCRUAL EARNINGS ON SECOND MONTH
      * IF "C" WHEN MORE THAN "SP-ERN-DAYS" DAYS ELAPSE IN THE 1ST MONTH
      *        TAKE EARNINGS THAT MONTH
      * IF "D" IF ACCRUAL DATE IS ON OR AFTER SAME AS CASH DATE TAKE FIRST
      *        SLICE, OTHERWISE LOGIC WILL ACT LIKE SP-ERNACCRL-FG(AERN) = " "

           IF (SP-ERNACCRL-FG(AERN) = "D")
              IF LN-CASHDATE NOT = 0
                 MOVE LN-CASHDATE  TO NUM-DATE
                 MOVE AERN-ENDDATE TO SYS-DATE
                 IF SYS-DATE < NUM-DATE
                    MOVE AERN-LN-ANTICTERM TO AERN-REMTERM
                    GO TO AERN-GET-REM-TERM-EXIT.

           IF (SP-ERNFRMLA(AERN) NOT = 56)
              IF (SP-ERNACCRL-FG(AERN) = " " OR "D")
                     OR (SP-ERNACCRL-FG(AERN) = "A"
                                       AND AERN-FLAG1 = 0)
                     OR (SP-ERN-DAYS(AERN) > 0
                             AND SP-ERNACCRL-FG(AERN) = "C"
                               AND ELAPSED-REM NOT < SP-ERN-DAYS(AERN)
                        )
                 SUBTRACT 1 FROM AERN-REMTERM.

           IF SP-ERNFRMLA(AERN) = 56
              IF SP-ERN-DAYS(AERN) > 0
                 IF ELAPSED-REM NOT < SP-ERN-DAYS(AERN)
                    SUBTRACT 1 FROM AERN-REMTERM.

           IF SP-ERNACCRL-DFCD(AERN) = "A"
              ADD AERN-LN-TOTNODEF TO AERN-REMTERM.

       AERN-GET-REM-TERM-EXIT.
           EXIT.

      ***************************************************
      *    (52) & (55)
      *    COMPUTE NEW APR FOR ALL CHARGES
      ***************************************************
       AERN-COMPUTE-ALLCHG-APR SECTION.
           PERFORM LOAN-CALCULATIONS.
           MOVE DISCLOSED-FINAMT TO AERN-WK-FINANCED.
           MOVE LN-ORGTERM TO AERN-WK-ORGTERM.

           COMPUTE AERN-WORKER ROUNDED =
                         LN-UFERN(1) + LN-UFERN(3) + LN-UFERN(4).

           COMPUTE AERN-WORKER ROUNDED =
                                AERN-WORKER / AERN-LN-ANTICTERM.
           PERFORM AERN-COMPUTE-APR-COMMON.

      ***************************************************
      *    COMPUTE NEW APR COMMON CODE
      ***************************************************
       AERN-COMPUTE-APR-COMMON SECTION.
           SUBTRACT AERN-WORKER FROM LN-REGPYAMT
                                   GIVING AERN-WK-REGPYAMT
                                          LN-REGPYAMT.
           SUBTRACT AERN-WORKER FROM LN-1STPYAMT
                                   GIVING AERN-WK-1STPYAMT.
           MOVE LN-1STPYAMT TO AERN-WK-1STPYAMT.

           IF LN-LASTPYAMT NOT = 0
              SUBTRACT AERN-WORKER FROM LN-LASTPYAMT
                                           GIVING LN-LASTPYAMT.
           MOVE LN-LASTPYAMT TO AERN-WK-LASTPYAMT.

           MOVE AERN-LN-INTDATE TO AERN-WK-INTDATE.
           MOVE AERN-LN-ORIG-1STPYDATE TO AERN-WK-ORIG-1STPYDATE.

           MOVE "A" TO AERN-WK-DISFRMLA.
           PERFORM AERN-CALC-APR-NEW.
           MOVE AERN-WK-APRATE TO LN-APRATE.

      **********************************************************
      *    52, 55, 60, 62 & 63
      *
      *    SET LOAN AND SPR WORKERS FOR OTHER THAN INTEREST
      **********************************************************
       AERN-SET-WORKERS-NEW SECTION.
      *--------------------------------------------------------------------
      *    FIELDS SET EXTERNALLY FOR ADDONS   VIA: AERN-ADDON-FG = "Y"
      *            OR IN THIS EARNINGS MODULE VIA: AERN-ADDON-FG = "N"
      *--------------------------------------------------------------------
           MOVE AERN-LN-ORIG-1STPYDATE TO AERN-WK-ORIG-1STPYDATE.
           MOVE AERN-LN-PURDATE        TO AERN-WK-PURDATE.
           MOVE AERN-LN-LOANDATE       TO AERN-WK-LOANDATE.
           MOVE AERN-LN-INTDATE        TO AERN-WK-INTDATE.
           MOVE AERN-LN-TOTNODEF       TO AERN-WK-TOTNODEF.
           MOVE AERN-LN-ANTICTERM      TO AERN-WK-ANTICTERM.
      *<-> MOVE AERN-LN-SALEFIN        TO AERN-WK-SALEFIN.
           MOVE AERN-LN-ANTICERN       TO AERN-WK-ANTICERN.
           MOVE AERN-LN-ANTICADJ       TO AERN-WK-ANTICADJ.
           MOVE AERN-LN-ACCERN         TO AERN-WK-ACAERN.
           MOVE AERN-LN-UFERN          TO AERN-WK-UFERN.
           MOVE AERN-LN-REBATE         TO AERN-WK-REBATE.
           SUBTRACT AERN-WK-UFERN FROM AERN-WK-ANTICERN
                                   GIVING AERN-WK-FINCHG.

      *---------------------------------------------------------
      *    FIELDS NOT SET FOR ADDONS
      *---------------------------------------------------------
           MOVE LN-1STPYDATE        TO AERN-WK-1STPYDATE.
           MOVE LN-ORGTERM          TO AERN-WK-ORGTERM.
           MOVE LN-SUM-ORGTERM      TO AERN-WK-SUM-ORGTERM.
           MOVE LN-1STPYAMT         TO AERN-WK-1STPYAMT.
           MOVE LN-NOREGPY          TO AERN-WK-NOREGPY.
           MOVE LN-REGPYAMT         TO AERN-WK-REGPYAMT.
           MOVE LN-LASTPYAMT        TO AERN-WK-LASTPYAMT.
           MOVE LN-ORIGPURAMT       TO AERN-WK-ORIGPURAMT.
           MOVE LN-APRATE           TO AERN-WK-APRATE.
           MOVE SP-DISFRMLA         TO AERN-WK-DISFRMLA.

      *************************************************
      *    SAVE OFF LOAN & SPR FIELDS
      *************************************************
       AERN-SAVE-LN-SP SECTION.
           MOVE LN-1STPYAMT         TO AERN-SV-1STPYAMT.
           MOVE LN-REGPYAMT         TO AERN-SV-REGPYAMT.
           MOVE LN-LASTPYAMT        TO AERN-SV-LASTPYAMT.
           MOVE LN-APRATE           TO AERN-SV-APRATE.
           MOVE LN-INTDATE          TO AERN-SV-INTDATE.
           MOVE LN-LOANDATE         TO AERN-SV-LOANDATE.
           MOVE LN-ORIG-1STPYDATE   TO AERN-SV-1STPYDATE.
           MOVE LN-ORGTERM          TO AERN-SV-ORGTERM.
           MOVE LN-NOREGPY          TO AERN-SV-NOREGPY.
           MOVE LN-ANTICTERM        TO AERN-SV-ANTICTERM.

           MOVE SP-REBTBL(7)        TO AERN-SV-REBTBL.
           MOVE SP-YEARTYPE         TO AERN-SV-YEARTYPE.
           MOVE SP-DISFRMLA         TO AERN-SV-DISFRMLA.

      *************************************************
      *    RESTORE LOAN & SPR FIELDS
      *************************************************
       AERN-RESTORE-LN-SP SECTION.
           MOVE AERN-SV-1STPYAMT    TO LN-1STPYAMT.
           MOVE AERN-SV-REGPYAMT    TO LN-REGPYAMT.
           MOVE AERN-SV-LASTPYAMT   TO LN-LASTPYAMT.
           MOVE AERN-SV-APRATE      TO LN-APRATE.
           MOVE AERN-SV-INTDATE     TO LN-INTDATE.
           MOVE AERN-SV-LOANDATE    TO LN-LOANDATE.
           MOVE AERN-SV-1STPYDATE   TO LN-ORIG-1STPYDATE.
           MOVE AERN-SV-ORGTERM     TO LN-ORGTERM.
           MOVE AERN-SV-NOREGPY     TO LN-NOREGPY.
           MOVE AERN-SV-ANTICTERM   TO LN-ANTICTERM.

           MOVE AERN-SV-REBTBL      TO SP-REBTBL(7).
           MOVE AERN-SV-YEARTYPE    TO SP-YEARTYPE.
           MOVE AERN-SV-DISFRMLA    TO SP-DISFRMLA.

      ***********************************************
      *    COMPUTE ACTUARIAL REGZ UNEARNED NEW
      ***********************************************
       AERN-ACTUARIAL-REGZ-NEW SECTION.
           MOVE AERN-ENDDATE           TO REGZ-DATE.
           MOVE AERN-WK-FINCHG         TO REGZ-FINCHG.
           MOVE AERN-WK-LOANDATE       TO REGZ-LOANDATE.
           MOVE AERN-WK-INTDATE        TO REGZ-INTDATE.
           MOVE AERN-WK-ORIG-1STPYDATE TO REGZ-1STPYDATE.
           MOVE AERN-WK-ORGTERM        TO REGZ-ORGTERM.
           MOVE AERN-WK-1STPYAMT       TO REGZ-1STPYAMT.
           MOVE AERN-WK-REGPYAMT       TO REGZ-REGPYAMT.
           MOVE AERN-WK-LASTPYAMT      TO REGZ-LASTPYAMT.
           MOVE AERN-WK-APRATE         TO REGZ-APRATE.
           MOVE AERN-WK-DISFRMLA       TO REGZ-DISFRMLA.
           MOVE LN-UNITPER-CD          TO REGZ-UNITPER-CD.
           MOVE LN-UNITPER-FREQ        TO REGZ-UNITPER-FREQ.
           MOVE AERN-WK-INCLUDE-DEF-FG TO REGZ-INCLUDE-DEF-FG.
           MOVE AERN-WK-TOTNODEF       TO REGZ-TOTNODEF.

           PERFORM REGZ-UNEARNED-CALC.

      ***************************************************
      *    GET NEW APR FOR COMPONENT
      ***************************************************
       AERN-APR-COMPONENT-NEW SECTION.
           IF NOT (SP-ERNFRMLA(AERN) = 52 OR 55 OR 65)
              GO TO AERN-APR-COMPONENT-NEW-SKIP.

      *-----------------------------------------------------------
      *-------  THIS IS TO MATCH OLD LOGIC FOR 52 & 55  ----------

      *  CORRECTED FOR CITIZENS, RE: MIDSTREAM PURCHASE AND ALL
      *  EARNINGS ARE PUT INTO DISCOUNT
           IF (AERN-LN-SALEFIN = "Y") AND
              (LN-ORIGPURAMT NOT = LN-LNAMT)
              GO TO AERN-APR-COMPONENT-NEW-SKIP.

           PERFORM LOAN-CALCULATIONS.
           ADD TOTAL-INSFINCHG TOTAL-FEEFINCHG DISCLOSED-FINAMT
                                GIVING AERN-WK-FINANCED.
           MOVE LN-ORGTERM TO AERN-WK-ORGTERM.

           COMPUTE AERN-WORKER ROUNDED =
                           LN-INTCHG + LN-SERCHG + LN-EXTCHG
                         + (LN-MAINTFEE * LN-ORGTERM)
                         + AERN-LN-UFERN
                         - (AERN-LN-ANTICERN + AERN-LN-ANTICADJ).

           COMPUTE AERN-WORKER ROUNDED =
                                AERN-WORKER / AERN-LN-ANTICTERM.

           SUBTRACT AERN-WORKER FROM LN-REGPYAMT GIVING AERN-WK-REGPYAMT.
           SUBTRACT AERN-WORKER FROM LN-1STPYAMT GIVING AERN-WK-1STPYAMT.
           IF LN-LASTPYAMT NOT = 0
              SUBTRACT AERN-WORKER FROM LN-LASTPYAMT
                                           GIVING AERN-WK-LASTPYAMT.

           MOVE "A" TO AERN-WK-DISFRMLA.

           GO TO AERN-APR-COMPONENT-NEW-EXIT.

      *-----------------------------------------------------------
       AERN-APR-COMPONENT-NEW-SKIP.

           IF AERN-LN-SALEFIN = "Y"
              MOVE AERN-WK-PURDATE TO AERN-WK-INTDATE
                                      AERN-WK-LOANDATE.

           IF SP-ERNFRMLA(AERN) = 62
              COMPUTE AERN-WK-FINCHG =
                  LN-ANTICERN(1) + LN-ANTICERN(3)
                   + LN-ANTICERN(4) + LN-ANTICERN(5)
                     + LN-ANTICERN(6)
              COMPUTE AERN-WK-FINANCED =
                 AERN-WK-1STPYAMT
                     + (AERN-WK-NOREGPY * AERN-WK-REGPYAMT)
                      + AERN-WK-LASTPYAMT
                       - (LN-ANTICERN(1) + LN-ANTICADJ(1))
                        - (LN-ANTICERN(3) + LN-ANTICADJ(3))
                         - (LN-ANTICERN(4) + LN-ANTICADJ(4))
                          - (LN-ANTICERN(5) + LN-ANTICADJ(5))
                           - (LN-ANTICERN(6) + LN-ANTICADJ(6))
           ELSE
           IF SP-ERNFRMLA(AERN) = 63
              COMPUTE AERN-WK-FINANCED =
                 AERN-WK-1STPYAMT
                     + (AERN-WK-NOREGPY * AERN-WK-REGPYAMT)
                      + AERN-WK-LASTPYAMT
                       - (LN-ANTICERN(1) + LN-ANTICADJ(1))
                          - (LN-ANTICERN(5) + LN-ANTICADJ(5))
           ELSE
              COMPUTE AERN-WK-FINANCED =
                 AERN-WK-1STPYAMT
                     + (AERN-WK-NOREGPY * AERN-WK-REGPYAMT)
                      + AERN-WK-LASTPYAMT
                       + AERN-WK-UFERN
                        - (AERN-WK-ANTICERN + AERN-WK-ANTICADJ).

       AERN-APR-COMPONENT-NEW-EXIT.
           MOVE " " TO AERN-WK-INCLUDE-DEF-FG.
           PERFORM AERN-CALC-APR-NEW.

      ***************************************************
      *    CALCULATE NEW APR
      ***************************************************
       AERN-CALC-APR-NEW SECTION.
           MOVE AERN-WK-1STPYAMT TO APR-1STPYAMT.
           MOVE AERN-WK-REGPYAMT TO APR-REGPYAMT.
           MOVE AERN-WK-LASTPYAMT TO APR-LASTPYAMT.
           MOVE AERN-WK-ORGTERM TO APR-ORGTERM.
           MOVE AERN-WK-INTDATE TO APR-INTDATE.
           MOVE AERN-WK-ORIG-1STPYDATE TO APR-1STPYDATE.
           MOVE AERN-WK-FINANCED TO APR-FINANCED.
           MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.

           MOVE AERN-WK-DISFRMLA TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE APR-APR TO AERN-WK-APRATE.

      ***************************************************
      *    SETUP FOR MIDSTREAM PAPER ACTUARIAL CALC
      ***************************************************
       AERN-MIDSTREAM-NEW SECTION.
           MOVE AERN-WK-PURDATE TO AERN-WK-INTDATE
                                   AERN-WK-LOANDATE.

      * DETERMINE ELAPSED TIME REMAINING BETWEEN
      * PURCHASE DATE AND CURRECT MATURITY EXCLUDING DEFERMENTS:
           MOVE "Y" TO AERN-MDTE-FLAG
                       AERN-MDTE-ORGTERM-FG.
           PERFORM AERN-MATURITY-DATE-CALC.

           MOVE AERN-WK-PURDATE TO NUM-DATE.
           MOVE AERN-MDTE-DATE TO SYS-DATE.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.

      * SET FRACTIONAL PERIOD FROM PAYDATE TO NEXT PAYMENT DUE DATE:
           MOVE ELAPSED-UNITPER-REM TO AERN-PPAYS.

      * SET WHOLE PERIOD(S) FROM PURCHASE DATE TO MATURITY:
           MOVE ELAPSED-UNITPER TO AERN-WPAYS.
           IF ELAPSED-UNITPER-REM NOT = 0
              ADD 1 TO AERN-WPAYS.

           IF AERN-WPAYS < AERN-WK-ORGTERM
              MOVE AERN-WPAYS TO AERN-WK-ANTICTERM
                                 AERN-WK-ORGTERM
              MOVE AERN-WK-REGPYAMT TO AERN-WK-1STPYAMT
              SUBTRACT 1 FROM AERN-WK-ORGTERM
                                        GIVING AERN-WK-NOREGPY
              IF AERN-WK-LASTPYAMT NOT = 0
                 SUBTRACT 1 FROM AERN-WK-NOREGPY
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE AERN-WK-ORGTERM TO AERN-WK-ANTICTERM.

      *    MOVE AERN-WK-ORIG-1STPYDATE TO NUM-DATE.
           MOVE AERN-WK-1STPYDATE TO NUM-DATE.
           MOVE AERN-WK-PURDATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER NOT < 0
              PERFORM AERN-MIDSTREAM-NEW-PUR-AFTER.

      * ADJUST 1ST PAYMENT FOR LOAN DELINQUENCY OR PAID AHEAD
      * AT MIDSTREAM PURCHASE TIME:
           COMPUTE AERN-WORKER =
               AERN-WK-ORIGPURAMT
                  - ( AERN-WK-NOREGPY * AERN-WK-REGPYAMT
                                         + AERN-WK-LASTPYAMT ).
           IF AERN-WORKER > 0
              MOVE AERN-WORKER TO AERN-WK-1STPYAMT
              MOVE AERN-WK-1STPYDATE TO AERN-WK-ORIG-1STPYDATE
              GO TO AERN-MIDSTREAM-NEW-EXIT.

      *---------------------------------------------------------
      *  EXAMPLE:
      *                       TERM = 1     TERM = 2     TERM = 6
      *                     -----------  -----------  -----------
      *   LN-1STPYAMT    :  $100.00      $100.00      $100.00
      *   LN-REGPYAMT    :  $100.00 X 0  $120.00 X 0  $120.00 X 4
      *   LN-LASTPYAMT   :  $  0.00      $120.00      $130.00
      *                      ------       ------       ------
      *      TOTAL REPAY :  $100.00      $220.00      $710.00
      *---------------------------------------------------------
           MOVE 0 TO AERN-WK-ANTICTERM
                     AERN-WK-ORGTERM.
           MOVE AERN-WK-ORIGPURAMT TO AERN-WORKER.
           IF AERN-WORKER > AERN-WK-LASTPYAMT
              ADD 1 TO AERN-WK-ANTICTERM
                       AERN-WK-ORGTERM
              SUBTRACT AERN-WK-LASTPYAMT FROM AERN-WORKER
           ELSE
      * SETUP FOR SINGLE PAY LOAN:
              MOVE AERN-WORKER TO AERN-WK-1STPYAMT
                                  AERN-WK-REGPYAMT
              MOVE 0 TO AERN-WK-NOREGPY
                        AERN-WK-LASTPYAMT
              MOVE 1 TO AERN-WK-ANTICTERM
                        AERN-WK-ORGTERM
              GO TO AERN-MIDSTREAM-NEW-1ST.
           DIVIDE AERN-WK-REGPYAMT INTO AERN-WORKER
                                         GIVING AERN-WORKNO.
           IF AERN-WORKNO = 0
      * SETUP FOR TWO PAY LOAN:
              MOVE AERN-WORKER TO AERN-WK-1STPYAMT
              MOVE AERN-WK-LASTPYAMT TO AERN-WK-REGPYAMT
              MOVE 0 TO AERN-WK-NOREGPY
              MOVE 2 TO AERN-WK-ANTICTERM
                        AERN-WK-ORGTERM
              GO TO AERN-MIDSTREAM-NEW-1ST.

           MOVE AERN-WORKNO TO AERN-WK-NOREGPY.
           COMPUTE AERN-WORKER =
                 AERN-WORKER - AERN-WORKNO * AERN-WK-REGPYAMT.
           ADD AERN-WORKNO TO AERN-WK-ANTICTERM
                              AERN-WK-ORGTERM.
           IF AERN-WORKER = 0
      * 1STPYAMT = REGPYAMT:
              MOVE AERN-WK-REGPYAMT TO AERN-WK-1STPYAMT
              SUBTRACT 1 FROM AERN-WK-NOREGPY
              GO TO AERN-MIDSTREAM-NEW-1ST.

      * FIRST PAYMENT:
           MOVE AERN-WORKER TO AERN-WK-1STPYAMT.
           ADD 1 TO AERN-WK-ANTICTERM
                    AERN-WK-ORGTERM.

        AERN-MIDSTREAM-NEW-1ST.
           MOVE AERN-MDTE-DATE TO NDTE-DATE.
           ADD 1 AERN-WK-NOREGPY GIVING NDTE-HOLD.
           IF AERN-WK-LASTPYAMT = 0
              SUBTRACT 1 FROM NDTE-HOLD.
           MULTIPLY -1 BY NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO AERN-WK-ORIG-1STPYDATE.

       AERN-MIDSTREAM-NEW-EXIT.
           EXIT.

       AERN-MIDSTREAM-NEW-PUR-AFTER SECTION.
      *    MOVE AERN-WK-ORIG-1STPYDATE TO NDTE-DATE.
           MOVE AERN-WK-1STPYDATE TO NDTE-DATE.
           MOVE ELAPSED-UNITPER TO NDTE-HOLD.
           IF ELAPSED-UNITPER-REM NOT < 0
              ADD 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO AERN-WK-1STPYDATE.

       AERN-MATURITY-DATE-CALC SECTION.
           MOVE AERN-WK-1STPYDATE TO NDTE-DATE.
           MOVE 0 TO NDTE-HOLD.
           IF AERN-MDTE-FLAG = " "
              MOVE AERN-WK-TOTNODEF TO NDTE-HOLD.

           IF AERN-MDTE-ORGTERM-FG = "Y"
              COMPUTE NDTE-HOLD = NDTE-HOLD + (AERN-WK-ORGTERM - 1)
           ELSE
              COMPUTE NDTE-HOLD = NDTE-HOLD + (AERN-WK-SUM-ORGTERM - 1).

           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO AERN-MDTE-DATE.
           MOVE " " TO AERN-MDTE-FLAG
                       AERN-MDTE-ORGTERM-FG.

      *-----------------------------------------------------------------------
      *    DUMMY SECTION TO INCLUDE IN COPY MEMBERS THAT NEED TO BE
      *    REFLECTED IN 'WHAT' COMMANDS FOR SUPPORT
      *-----------------------------------------------------------------------
       SCCS-COPY-ROUTINE SECTION. MOVE
           "@(#)./LIBLP/LPAERN.CPY S35 >"
              TO MESS.

      *-----------------------------------------------------------------------
