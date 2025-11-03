      * COPYMEMBER: LIBWI/BUYRTEW
      * REV:
      *  2024.0415 BAH REMOVED BUYRTE-APRATE
      **************************************************************************
      *                   LIBWI/BUYRTEW
      *
      *     WORKERS FOR LONPB0.C AND SFQUOT.C
      *
      * JTG 030613 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *            RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *            AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      **************************************************************************
       01  BUYRTE-BUF.
           03  BUYRTE-BUYRATE        PIC 9(3)V9(3).

