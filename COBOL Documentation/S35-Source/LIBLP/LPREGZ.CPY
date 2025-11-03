      * COPYMEMBER: LIBLP/LPREGZ
      *****************************************************************
      *         ACTUARIAL / U.S. RULE UNEARNED ROUTINE
      *
      *         FEDERAL RESERVE REGULATION Z METHOD
      *
      *   NAME: LPREGZ
      *   DESC: CALCULATES THE UNEARNED (REFUND) ACCORDING TO
      *         FEDERAL RESERVE REGULATION Z
      *
      *   IN  : REGZ-DATE              - DATE OF UNEARNED
      *         REGZ-LOANDATE          - LOAN DATE
      *         REGZ-INTDATE           - INTEREST DATE
      *         REGZ-1STPYDATE         - 1ST PAYMENT DATE
      *         REGZ-ORGTERM           - TERM OF LOAN
      *         REGZ-1STPYAMT          - AMOUNT OF 1ST PAYMENT
      *         REGZ-REGPYAMT          - AMOUNT OF REGULAR PAYMENTS
      *         REGZ-LASTPYAMT      - THE AMOUNT OF THE LAST PAYMENT
      *         REGZ-FINCHG            - THE FINANCE CHARGE
      *         REGZ-APRATE            - ANNUAL PERCENTAGE RATE
      *         REGZ-DISFRMLA          - DISCLOSURE FORMULA
      *                                     A - ACTUARIAL REGZ
      *                                     C - U.S. RULE
      *         REGZ-UNITPER-CD        - UNIT PERIOD CODE      EX: "W"
      *         REGZ-UNITPER-FREQ      - UNIT PERIOD FREQUENCY EX: 1
      *         REGZ-INCLUDE-DEF-FG    - SHOULD DEFERMENTS AFFECT OUTCOME
      *                                    " " - NO (DEFAULT)
      *                                    "Y" - YES
      *         REGZ-TOTNODEF          - TOTAL NO. OF DEFERMENTS
      *
      *   OUT : REGZ-UNEARNED
      *         REGZ-PRES-VALUE
      *
      *   COPY:
      * REV :
      *****************************************************************
       REGZ-UNEARNED-CALC SECTION.
           MOVE 0 TO REGZ-UNEARNED
                     REGZ-PRES-VALUE.
           IF REGZ-APRATE = 0 OR REGZ-APRATE = 0
              MOVE REGZ-FINCHG TO REGZ-UNEARNED
              GO TO REGZ-UNEARNED-CALC-EXIT.

      ***************************************************
      * DETERMINE IF UNEARNED DATE IS BEFORE INTDATE
      ***************************************************
           MOVE REGZ-INTDATE TO NUM-DATE.
           MOVE REGZ-DATE    TO SYS-DATE.
           IF SYS-DATE < NUM-DATE
              MOVE REGZ-FINCHG TO REGZ-UNEARNED
              GO TO REGZ-UNEARNED-CALC-EXIT.

      ***************************************************
      * SET UNIT PERIOD INFORMATION
      ***************************************************
            MOVE REGZ-UNITPER-CD TO UPER-UNITPER-CD.
            MOVE REGZ-UNITPER-FREQ TO UPER-UNITPER-FREQ.
            MOVE REGZ-INTDATE TO UPER-INTDATE.
            MOVE REGZ-1STPYDATE TO UPER-1STPYDATE.
            MOVE REGZ-ORGTERM TO UPER-TERM.
            PERFORM UPER-CALCULATION.

      ****************************************************
      * DETERMINE ELAPSED TIME REMAINING BETWEEN
      *           UNEARNED DATE AND ORIGINAL MATURITY
      ****************************************************
           MOVE REGZ-1STPYDATE TO NDTE-DATE.
           SUBTRACT 1 FROM REGZ-ORGTERM GIVING NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE REGZ-DATE TO NUM-DATE.
           MOVE NDTE-DATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF SYS-DATE <= NUM-DATE
              GO TO REGZ-UNEARNED-CALC-EXIT.

      *****************************************************
      * SET FRACTIONAL PERIOD FROM UNEARNED DATE TO NEXT
      *     PAYMENT DUE DATE:
      *****************************************************
           COMPUTE REGZF = ELAPSED-UNITPER-REM
                               / UPER-DAYS-IN-UNITPER

      *****************************************************
      * SET WHOLE PERIOD(S) FROM UNEARNED DATE TO NEXT
      *     PAYMENT DUE DATE:
      *****************************************************
           IF ELAPSED-UNITPER NOT < REGZ-ORGTERM
              COMPUTE REGZT = ELAPSED-UNITPER - REGZ-ORGTERM + 1
           ELSE
              IF REGZF = 0
                 MOVE 1 TO REGZT
              ELSE
                 MOVE 0 TO REGZT.

      *************************************************
      * COMPUTE INT. RATE PER PMT/COMPOUND INTERVAL
      *************************************************
           COMPUTE REGZI ROUNDED =
              REGZ-APRATE * 0.01 / UPER-UNITPER-PER-YEAR.

      ************************************************************
      * COMPUTE 1ST PAYMENT REQUIREMENTS AND ADJUSTMENT FACTOR
      ************************************************************
      * SINGLE PAY LOANS AND US RULE:
           IF REGZ-ORGTERM < 2 OR REGZ-DISFRMLA = "C"
              COMPUTE REGZADJ ROUNDED =
                 1 / (1 + (REGZT + REGZF) * REGZI)
           ELSE
      * ACTUARIAL:
              COMPUTE REGZADJ ROUNDED =
                 1 / ((1 + REGZF * REGZI) * (1 + REGZI) ** REGZT).

           MOVE REGZ-1STPYAMT TO REGZ1P.
           IF ELAPSED-UNITPER > REGZ-ORGTERM
              MOVE REGZ-ORGTERM TO ELAPSED-UNITPER
           ELSE
              IF ELAPSED-UNITPER < (REGZ-ORGTERM - 1)
                 MOVE REGZ-REGPYAMT TO REGZ1P
              ELSE
                 IF ELAPSED-UNITPER = (REGZ-ORGTERM - 1)
                    IF ELAPSED-UNITPER-REM = 0
                       MOVE REGZ-REGPYAMT TO REGZ1P.

      ***********************************************************
      * COMPUTE REGULAR PAYMENT REQUIREMENTS & ANNUITY FACTOR
      ***********************************************************
           IF REGZ-ORGTERM < 3
              MOVE 0 TO REGZP-FAC
                        REGZP-N
              GO TO REGZ-UNEARNED-CALC-BALLOON.
           MOVE REGZ-REGPYAMT TO REGZP.
           IF ELAPSED-UNITPER-REM = 0
                            OR ELAPSED-UNITPER = REGZ-ORGTERM
              SUBTRACT 2 FROM ELAPSED-UNITPER GIVING REGZP-N
           ELSE
              SUBTRACT 1 FROM ELAPSED-UNITPER GIVING REGZP-N.
           IF REGZP-N < 0
              MOVE 0 TO REGZP-N.

           COMPUTE REGZP-FAC ROUNDED =
                     (1 - 1 / (1 + REGZI) ** REGZP-N) / REGZI.

      ***************************************************
      * COMPUTE BALLOON REQUIREMENTS & PAYMENT FACTOR
      ***************************************************
       REGZ-UNEARNED-CALC-BALLOON.
           IF REGZ-ORGTERM < 2
              MOVE 0 TO REGZBP
              GO TO REGZ-UNEARNED-CALC-PV.

           MOVE REGZ-LASTPYAMT TO REGZBP.
           IF ELAPSED-UNITPER < 1
              MOVE 0 TO REGZBP-N
                        REGZ1P
           ELSE
           IF ELAPSED-UNITPER = 1
              IF ELAPSED-UNITPER-REM = 0
                 MOVE 0 TO REGZBP-N
                           REGZ1P
              ELSE
                 MOVE 1 TO REGZBP-N
           ELSE
              IF ELAPSED-UNITPER-REM = 0
                            OR ELAPSED-UNITPER = REGZ-ORGTERM
                 SUBTRACT 1 FROM ELAPSED-UNITPER GIVING REGZBP-N
              ELSE
                 MOVE ELAPSED-UNITPER TO REGZBP-N.

           COMPUTE REGZBP-FAC ROUNDED =
                      1 / ((1 + REGZI) ** REGZBP-N).

      ***************************************************
      * COMPUTE PRESENT VALUE OF REMAINING PAYMENTS
      ***************************************************
       REGZ-UNEARNED-CALC-PV.
           COMPUTE REGZPV ROUNDED =
              REGZADJ * (REGZ1P + REGZP * REGZP-FAC
                                 + REGZBP * REGZBP-FAC).
           MOVE REGZPV TO REGZ-PRES-VALUE.

      ***************************************************
      * COMPUTE UNEARNED AMOUNT:
      ***************************************************
           COMPUTE REGZ-UNEARNED ROUNDED =
              (REGZ1P + REGZP * REGZP-N + REGZBP) - REGZPV.

       REGZ-UNEARNED-CALC-EXIT.
           EXIT.
