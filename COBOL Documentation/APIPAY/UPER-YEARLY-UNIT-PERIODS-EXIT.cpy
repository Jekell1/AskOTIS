       UPER-YEARLY-UNIT-PERIODS-EXIT.
           EXIT.
      *================================================================*
      * END COPYBOOK: LIBLP\LPUPER.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPPDUE.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPPDUE
      ***********************************************************************
      *          PAST DUE DAYS CALCULATION
      *
      *    NAME: LPPDUE
      *    DESC: ROUTINE COMPUTES DAYS PAST DUE FROM
      *          PAID THRU DATE
      *    IN  : PDUE-TODAY-DATE        (TODAYS DATE)
      *          PDUE-1STPYDATE    (LN-1STPYDATE)
      *    OUT : PDUE-DAYS
      *    USED: LN-CURBAL, LN-PAYFREQ
      *    COPY: LPPDUEW, LPMDTEW, LPMDTE, SPPDTHW, SPPDTH
      * REV:
      *  050986 JTG TOOK OUT CHECK ON PAST MATURITY
      *             WHICH CAUSED INPROPER VIEW OF
      *             PAST DUE DAYS EVEN THOUGH ALL
      *             PAYMENTS BECOME DUE THEN.
      *  041289 SLC ADDED CODE TO ADJUST THE DUE DATE
      *             TO AVOID MONTHEND PROBLEMS.
      *  041693 JTG ADDED LOGIC FOR UNIT PERIODS
      *  040495 JTG CORRECT LOGIC FOR FEB OVER 28TH
      *  JTG 030307 PUT BACK LOGIC FOR TESTING MATURITY:
      *             WHEN PAID-THRU IS EQUAL TO MATURITY AND THE ACCOUNT
      *             STILL HAS A BALANCE, COUNT PAST DUE DAYS FROM MATURITY
      *             AND NOT FROM NEXT DUE DATE                  CREATIVE #398
      *  JTG 030313 CORRECTED CHANGE DONE ON 030307 TO EXAMINE
      *             WHEN GP-PDTH-GT-SCHED-PAY = 'Y'
      *             PAID THRU IS CAPPED AT THE NUMBER OF SCHEDULED PAYS
      *             SO SEE IF PAID-THRU IS AT THE CAP (MATURITY)
      *             AND COUNT PAST DUE DAYS FROM MATURITY
      *             NOT FROM MATURITY + 1 PAY FREQUENCY         CREATIVE #398
      *   CS 130327 PAST DUE DAYS WERE DISPLAYING AS 1 ON 3/14 WHEN
      *             PDTH-DATE-FULL 2/28 & INCREMENT-UNITPER INCREMENTED TO
      *             3/13 (1STPYDA 30) SHOULD BE 3/15. FORCED IF PAID THRU
      *             2/28 SUBTRACT NDTE-DD (28) FROM LN-1STPYDA =2, ADD 2 TO
      *             THE INCORRECT 3/13/ DATE YIELDING 3/15 AND PDUE-DAYS SHOULD
      *             BE 0   SOAUTO PL#793
      *   CS 160318 FIXED BUG IN ABOVE, THIS TEST AND SUBTRACT
      *             (SUBTRACT NDTE-DD FROM LN-1STPYDA) SHOULD ONLY BE DONE
      *             WHEN LN-1STPYDA > 28 #922
      ***********************************************************************
