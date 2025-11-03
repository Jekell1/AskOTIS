      ****************************************************
      *         PAYOFF-INT-ADDONS-GET-LP-REC
      *
      *    DUMMY SECTION USED BY LONPG7.C
      *    IT MUST BE THE LAST LINE OF THIS COPY MEMBER
      *    TO ALLOW LONPG7.C TO PLACE ITS LOGIC TO READ
      *    PAYMENT RECORDS.
      ****************************************************
       PAYOFF-INT-ADDONS-GET-LP-REC SECTION.

      *================================================================*
      * END COPYBOOK: LIBLP\LPPOF2.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\AGEING.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/AGEING
      *************************************************************************
      *   NAME:  AGEING - COMPUTE-RECENCY
      *   DESC:  GIVEN AGEING DATE (MMDDYY) COMPUTE
      *          RECENCY DAYS AND TABLE SUBSCRIPT
      *          FROM LAST PMT, OR LOAN DATE, IF NO
      *          PAYMENTS HAVE BEEN MADE
      *   IN  :  AGEING-DATE
      *   OUT :  RSUB
      *          RECENCY
      *          RDSUB
      *          RECDEL
      *          RDPOT30        ' ' = N/A
      *                         'Y' = POTENTIAL 30
      *          RDPOTCHGOFF    ' ' = N/A
      *                         'Y' = POTENTIAL CHARGE OFF
      *                         'M' = MISSED CHARGE OFF
      *   USED:  LN-LOANDATE, LN-DATE-PAID-LAST
      *-----------------------------------------------
      *
      *   RSUB
      *   RDSUB     INTERNAL          EXTERNAL
      *   -----     ------------      ------------
      *     1   -   CUR               30
      *     2   -    30               60
      *     3   -    60               90
      *     4   -    90              120
      *     5   -   120              150
      *     6   -   150              180
      *     7   -   180              210+
      *     8   -   210+             N/A
      *     9   -   N/A              POTENTIAL 30
      *    10   -   POTENTIAL 30     CURRENT
      *-----------------------------------------------
      * REV :
      *  JTG 072195 ADDED LOGIC FOR SP-RDPOT30-CD = 'A'
      *                             SP-RDPOTCHGOFF-CD = 'A'
      *  MJD 071595 ADDED A50 LOGIC FOR SP-BANKUNITPER-CD = "A".
      *  BLV 041097 IF SP PAY FREQ CODE = "A" (INSTANT), FORCE 100%
      *             TO PDTHRU % FOR COMPUTING PD THRU FOR AGEING; TO FIX:
      *             AGEING-HOLD-REMAIN > 50%  CAUSE PDTHRU% IS <100, SO
      *             90 ACCT IS AGED AS 60; 60 IS AGED AS 30, ETC.
      *  JTG 091597 ADDED LOGIC FOR NEW STAT/AGEING BUCKETS
      *  JTG 091697 ADDED LOGIC FOR 'D' WORLD A90, SP-BANKRULE & SP-TRWBANKRULE
      *  JTG 990419 ADDED SP-RECFRMLA-CALENDAR-DUEDAY  "D"         FROM REL: A50
      *                   SP-RECFRMLA-PERCNT-PMT       "P"         MERCURY  #183
      *  MJD 090427 ADDED SP-BANKRULE "E"                  (BARNETT) 71284 #3189
      *  MJD 110823 ADDED SP-BANKRULE "F" MX NON MONTHLY   (WORLD) #752
      *  MJD 120402 FIXED BAD 'IF' STATEMENT FOR BANKRULE "D" AND "F"
      *  BLM 160615 FOR BANKRULE R & S, CALL GETLTR SINCE REVOLVING FIELDS 
      *             ARE NOW IN LTRFILE, NOT LNFILE, PD0003
      * KEC 2016.1116 ADDED LTR-SEQNO TO LTR1-KEY.
      * BLM 2017.0208 FIX LOGIC USING LN-PY-YYYYMM-RECDEL FOR 8 DIGIT DATES
      * BAH 2017.0914 RENAMED LN-PYMMYY-RECDEL TO LN-PY-YYYYMM-RECDEL AND FIXED
      *               LOGIC TO USE IT CORRECTLY
      * BAH 2019.0626 REMOVED BANKRULE "R" AND "S" REVOLVING
      *************************************************************************
