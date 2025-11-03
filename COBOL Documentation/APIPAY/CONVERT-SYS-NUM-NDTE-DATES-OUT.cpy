      ***************************************************************
      * NAME: CONVERT-SYS-NUM-NDTE-DATES-OUT
      * DESC: CONVERT SYS/NUM/NDTE-DATE DATE FROM 00MMDDYY TO CCYYMMDD
      *       USED STRICTLY FOR TESTING WHILE INCEMENTALLY CHANING DATE
      *       FIELDS AND FORMATS.
      *       ALLOWS INTERNAL USAGE OF CCYYMMDD WHILE EXTERNAL FORMAT
      *       CAN BE CCYYMMDD OR 00MMDDYY
      * IN  : SYS-DATE (CCYYMMDD)
      *       NUM-DATE (CCYYMMDD)
      *       NDTE-DATE (CCYYMMDD)
      * OUT : SYS-DATE (00MMDDYY)
      *       NUM-DATE (00MMDDYY)
      *       NDTE-DATE (00MMDDYY)
      ***************************************************************
       CONVERT-SYS-NUM-NDTE-DATES-OUT SECTION.
           IF SYS-FG = "Y"
              MOVE SYS-DATE TO CONVERT-DATE
              PERFORM CONVERT-CCYYMMDD-TO-00MMDDYY
              MOVE CONVERT-DATE TO SYS-DATE.

           IF NUM-FG = "Y"
              MOVE NUM-DATE TO CONVERT-DATE
              PERFORM CONVERT-CCYYMMDD-TO-00MMDDYY
              MOVE CONVERT-DATE TO NUM-DATE.

           IF NDTE-FG = "Y"
              MOVE NDTE-DATE TO CONVERT-DATE
              PERFORM CONVERT-CCYYMMDD-TO-00MMDDYY
              MOVE CONVERT-DATE TO NDTE-DATE.
      *================================================================*
      * END COPYBOOK: LIBGB\DATER.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\DEFPOL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/DEFPOL
      ******************************************************************
      *          DEFERMENT POLICY VIOLATION TEST
      *
      *    NAME: DEFPOL
      *    DESC: THIS ROUTINE TEST FOR DEFERMENT POLICY VIOLATIONS
      *
      *    IN  : DEFPOL-TRCD        TRANS CODE, EX. 'DF', 'D2'
      *          DEFPOL-DATE        DATE OF DEFERMENT TEST
      *
      *    OUT : DEFPOL-MAXIMUM     ' '  - DEFERMENT WITHIN POLICY
      *                             'Y'  - DEFERMENT AGAINST POLICY
      *                             'X'  - DEFERMENT EXCEEDS LOAN TERM
      *                             '1'  - DEFERMENT WITHIN 1ST XMONTHS
      *                             'D'  - DEFERMENT MONTHS BETWEEN
      *                             '2'  - DEFERMENT D2-D9 MONTHS BETWEEN
      *                             'R'  - LR DOES NOT EXIST
      *    COPY: DEFPOLW
      *  REV:
      * JTG 111197 ADDED BR-NODEF-1ST-X-MONTHS OPTION MERCURY
      * MJD 991220 ADDED SP-DEFPOLCD = "B" (WORLD PR#216)
      * BAH 070201 ADDED BR-MIN-MONTHS-DEF FOR REGACC PR# 162
      * BAH 070503 CHANGED 365 TO 360 FOR ABOVE REGACC TEST, PL# 588
      *  CS 070808 ADDED TEST IF BR-MIN-MONTHS-DEF NOT NUMERIC, SKIP READING
      *            FOR THE LR. MULLEN
      *  CS 080408 ADDED TESTS FOR BR-MIN-MONTHS-D2, IF A MULTIPLE(D2-D9)
      *            IS POSTED, MUST WAIT BR-MIN-MONTHS-D2 TO POST ANOTHER
      *            DF TRANSACTION REGACC PR#202
      *  CS 080408 REGACC WANTS THE FLAG BR-NODEF-1ST-X-MONTHS TO BE
      *            PAYS PAID RATHER THAN ELAPSED TIME REGACC PR#202
      *  CS 120806 ADDED DEFPOL-MAXIMUM "1","2","D"  REGACC PR#3189
      *            "1", ATTEMPTING DF IN VIOLATION OF 1ST XMONTHS
      *            "D", MONTHS BETWEEN 'DF' & CURRENT DEFERMENT/S VIOLATION
      *            "2", MONTHS BETWEEN 'D2' AND CURRENT DEFERMENT/S VIOLATION
      *            "D" AND "2" COME FROM THE 'LR', IF NO 'LR' SETS 'R'
      * BAH 131017 ADDED SP-DEFPOLCD "C" FOR HONOR, PR# 4382
      * BAH 151021 REMOVE B2FILE, PD0003
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 20220803 HARD CODED START
      ******************************************************************
