       ADVANCE-SCHED SECTION.
           ADD 1 TO SCHD-SUB.

       EXIT-ADVANCE-SCHED.
           EXIT.
      *================================================================*
      * END COPYBOOK: LIBLP\LPSCHD.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPCEPP.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPCEPP
      **********************************************************
      *         COMPUTE CONTRACTUAL ELAPSED PAY PERIODS
      *
      *   NAME: LPCEPP
      *   DESC: THIS ROUTINE RETURNS THE NUMBER OF
      *         ELAPSED (CONTRACTUAL) PAY PERIODS
      *         AND THE REMAINING PORTION, IF ANY, IN DAYS.
      *
      *         DEFERRED AND ALLOWABLE DELINQUENT PERIODS
      *         ARE NOT CONSIDERED BY DEFAULT.
      *
      *         CALCULATIONS ARE CAPPED AT MATURITY
      *         BY DEFAULT.
      *
      *         NOTE:
      *               IF CEPP-INCDEF-FG HAS A VALUE
      *               OTHER THAN SPACES, DEFERRED
      *               PERIODS WILL BE CONSIDERED.
      *
      *               IF CEPP-INCALDEL-FG HAS A VALUE
      *               OTHER THAN SPACES, ALLOWABLE
      *               DELINQUENT PERIODS WILL BE CONSIDERED.
      *
      *               IF CEPP-NOCAP-FG HAS A VALUE
      *               OTHER THAN SPACES, ELAPSED
      *               PERIODS WILL NOT BE CAPPED
      *               AT ORIGINAL TERM.
      *
      *   IN  : CEPP1-DATE     (LN-1STPYDATE)
      *         CEPP2-DATE     (TODAYS DATE)
      *   OUT : CEPP-PERIODS
      *         CEPP-UNITPER-REM
      *
      *         N O T E:
      *                  CEPP1-DATE AND CEPP2-DATE NOT ALTERED
      *   USED:
      *   COPY: LPCEPPW
      * REV:
      *  031286 JTG STOPPED ALTERATION OF CEPPX-DATES.
      *  050986 JTG ADDED TEST ON CEPP-NOCAP-FG
      *             SO CONTRACTUAL AGEING DONE BY
      *             AGEING.IN.C WOULD COMPUTE CORRECTLY.
      *  052386 JTG ADDED LINE TO CLEAR CEPP-NOCAP-FG
      *  101686 JTG CHANGED TIME FROM 365 TO 360
      *             TO YIELD TRUE MONTHLY INTERVALS
      *  020287 JTG ADDED THE OPTION OF INCLUDING
      *             DEFERMENTS IN THE CALCULATION.
      *  040590 JTG INCLUDED LN-TOTNOALDEL IN CALCULATION
      *             WITH OPTIONAL DEFERMENTS FOR PACESETTER
      *  071696 JTG ADDED LOGIC FOR UNIT PERIODS
      *             ADDED RETURN OF CEPP-UNITPER-REM (CAC) FL
      *  090696 JTG CORRECTED BUG YIELDING 1 ELAPSED PAY PERIOD
      *             WHEN TODAY IS PRIOR TO FIRST PAY DUE DATE
      **********************************************************
