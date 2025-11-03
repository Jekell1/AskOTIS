      * COPYMEMBER: LIBLP/CALCPV
      *****************************************************************
      *         COMPUTE PRESENT VALUE (AMT FINANCED)
      *
      *   NAME: CALCPV
      *   DESC: COMPUTE THE PRESENT VALUE (AMT FINANCED)
      *
      *   IN  : CALCPV-INTDATE
      *         CALCPV-1STPYDATE
      *         CALCPV-TERM
      *         CALCPV-1STPYAMT   AMOUNT OF 1ST PAYMENT
      *         CALCPV-REGPYAMT
      *         CALCPV-LASTPYAMT     THE AMOUNT OF THE LAST PAYMENT
      *         CALCPV-UNITPER-CD
      *         CALCPV-UNITPER-FREQ
      *         CALCPV-RATE
      *         CALCPV-DISFRMLA = "A" OR "B" OR "C"
      *
      *   OUT : CALCPV-FINANCED   (AMOUNT FINANCED)
      *   COPY:
      * REV :
      *****************************************************************
       CALCPV-AMT-FINANCED SECTION.
           MOVE 0 TO CALCPV-FINANCED.
           IF (CALCPV-TERM = 0)
            OR (CALCPV-UNITPER-FREQ = 0)
             OR (NOT (CALCPV-UNITPER-CD = "M" OR "S" OR "W"
                                        OR "D" OR "Y" OR "B")
                )
              OR (CALCPV-1STPYAMT = 0)
               OR (CALCPV-RATE = 0)
                OR (CALCPV-LASTPYAMT NOT NUMERIC)
                    GO TO CALCPV-AMT-FINANCED-EXIT.

      * SETUP UNIT PERIOD INFORMATION:
           MOVE CALCPV-TERM         TO UPER-TERM.
           MOVE CALCPV-UNITPER-CD   TO UPER-UNITPER-CD.
           MOVE CALCPV-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE CALCPV-INTDATE      TO UPER-INTDATE.
           MOVE CALCPV-1STPYDATE    TO UPER-1STPYDATE.
           PERFORM UPER-CALCULATION.
           IF UPER-ERRCD NOT = " "
              GO TO CALCPV-AMT-FINANCED-EXIT.

      **********************************************************
      *    COMPUTE (CALCPV-I) INT. RATE PER UNIT PERIOD
      **********************************************************
           COMPUTE CALCPV-I ROUNDED =
              CALCPV-RATE / (100 * UPER-UNITPER-PER-YEAR).

      *********************************************************
      *    SET (CALCPV-F) FRACTIONAL PERIOD FROM INTDATE TO FIRST
      *    PAYMENT DUE DATE:
      *********************************************************
           MOVE UPER-FRACT-UNITPER TO CALCPV-F.

      *******************************************************
      *    SET (CALCPV-W) WHOLE PERIOD(S) FROM INTDATE TO FIRST
      *    PAYMENT DUE DATE:
      *******************************************************
           MOVE UPER-FULL-UNITPER TO CALCPV-W.

      *************************************************************
      *    COMPUTE (CALCPV-ADJ) ADJUSTMENT FACTOR TO 1ST DUE PAYMENT:
      *************************************************************
      * ACTUARIAL:
              COMPUTE CALCPV-ADJ ROUNDED =
                 1 / ((1 + CALCPV-F * CALCPV-I)
                              * (1 + CALCPV-I) ** CALCPV-W).

      ***************************************************
      *    ESTABLISH ANNUITY PAYMENTS REQUIREMENTS:
      ***************************************************
           IF CALCPV-TERM < 3
              MOVE 0 TO CALCPV-ANI
           ELSE
              COMPUTE CALCPV-ANI ROUNDED =
              (1 - 1 / (1 + CALCPV-I) ** (CALCPV-TERM - 2))
                                                       / CALCPV-I.

      ***************************************************
      *    ESTABLISH BALLOON FACTOR
      ***************************************************
           IF CALCPV-TERM = 1
              MOVE 0 TO CALCPV-LASTPYAMT-F
           ELSE
              COMPUTE CALCPV-LASTPYAMT-F ROUNDED =
                  (1 + CALCPV-I) ** (CALCPV-TERM - 1).

      ***************************************************
      *    COMPUTE (PRESENT VALUE) AMOUNT FINANCED
      ***************************************************
           COMPUTE CALCPV-FINANCED ROUNDED =
                CALCPV-ADJ
                   * (CALCPV-1STPYAMT
                        + CALCPV-REGPYAMT * CALCPV-ANI
                         + CALCPV-LASTPYAMT / CALCPV-LASTPYAMT-F
                     ).

       CALCPV-AMT-FINANCED-EXIT.
           EXIT.
