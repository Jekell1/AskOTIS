      *================================================================*
      * END COPYBOOK: LIBLP\LPXDTE.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLCAS.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLCAS
      **********************************************************************
      *         LATE CHARGE ASSESS ROUTINE
      *
      *   NAME: LPLCAS
      *   DESC: THIS ROUTINE TESTS, BASED ON THE PAYDATE,
      *         IF LATE CHARGES SHOULD BE ASSESSED. IT
      *         CONSIDERS GRACE DAYS AND CURRENT LATE
      *         CHARGE PAID THRU.
      *   IN  : LCAS-PAYDATE       (LP-PAYDATE)
      *         LCAS-1STPYDATE     (LN-1STPYDATE)
      *         LCAS-LCPDTH-DATE        (LN-LCPDTH)
      *   OUT : LCAS-ASSESS        ("Y" OR " ")
      *         LCAS-ELAPSED-DAYS
      *         LCAS-GRACE-DATE
      *   USED: LN-ORGTERM, LN-DEFMNTS
      *   COPY: LPMDTEW, LPNDTE, LPNDTEW, TIMALL, JUL, CJUL.
      * REV:
      *  SLC 090889 EXTENDED LAST GRACE DAY TO NEXT BUSINESS DAY
      *             IF IT FALLS ON SATURDAY, SUNDAY, OR A HOLIDAY
      *             FROM THE HOLIDAY TABLE IN THE GLOBAL.
      *  SLC 121289 ADDED ON OVERFLOW CHECK WHEN CALLING GLOBRD.
      *  JTG 120590 ADDED OUTPUT OF LCAS-ELAPSED-DAYS
      *  JTG 060592 ADDED LOGIC FOR LN-LC-CODE (ROSE SHANIS)
      *  JTG 080795 CORRECTED FOR HOLIDAY/WEEKEND TEST NEVER ENDING LOOP
      *  JTG 071696 ADDED LOGIC FOR UNIT PERIODS
      *  JTG 103096 CHANGED TEST ON LN-LC-CODEX TO TEST FOR "00", WHICH
      *             SHOULD ELIMINATE UNNECESSARY IO ON EL-FILE
      *  JTG 030497 CHANGED TO CORRECT, DUE DAY = 30TH & LCPDTH = 02289X
      *             CAUSING NEXT DUE TO BE 032897 AND NOT 033097
      *  JTG 122198 ADDED LOGIC TO RETURN LCAS-GRACE-DATE, REGENCY REVOLVING
      *  JTG 990121 CORRECTED LOGIC TO RETURN LCAS-GRACE-DATE, REGENCY REVOLVING
      * KEC 190219 A32 FFR - REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      * BAH 20220818 REMOVED CALL TO GLOBRD, EACH PROGRAM WILL READ GBFILE NOW
      **************************************************************************
      *******************************************
       LATE-ASSESS-ROUTINE SECTION.
      *******************************************
           MOVE " " TO LCAS-ASSESS.
           MOVE 0 TO LCAS-ELAPSED-DAYS
                     LCAS-GRACE-DATE.

           IF LCAS-1STPYDATE = 0
                      OR LCAS-PAYDATE = 0
              GO TO LATE-ASSESS-EXIT.

           IF SP-LCUNITPER-CD = "A"
              MOVE LN-UNITPER-CD TO UPWK-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO UPWK-UNITPER-FREQ
              PERFORM UPWK-LC-CALC
              COMPUTE LCAS-LCGRACE =
                   SP-LCGRACE * UPWK-LC-GRACE-FAC * .01
           ELSE
              MOVE SP-LCGRACE TO LCAS-LCGRACE.

           IF ( LN-LC-CODE NOT = ZEROES )
              PERFORM LATE-CHARGE-TEST-ELREAD.

      * SETUP NEXT PAYMENT DUE DATE:
           MOVE LCAS-LCPDTH-DATE TO NDTE-DATE
                               NUM-DATE.

      * ADDED TO CORRECTED DUE DAY = 30, LCPDTH = 02289X
           IF (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1)
              IF (NDTE-DD < LN-1STPYDA)
                 MOVE LN-1STPYDA TO NDTE-DD.
      * ADDED TO CORRECTED DUE DAY = 30, LCPDTH = 02289X

           MOVE 1 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
      * WHEN SEMI MONTHLY WITH FIRST PAYDATE OF 14TH OR 15TH,
      *      THE INCREMENT OF LCPDTH WILL GO TO 02/28/YY OR 02/29/YY
      *      WHEN LCPDTH WAS INCREMENTED FROM 02/14/YY OR 02/15/YY.
      *      NOW TO DETERMINE INTERNALLY IF THE PAYMENT IS LATE
      *      WE NEED TO SET LCPDTH BACK TO 03/14 OR 03/15
           IF LN-UNITPER-CD = "S"
              IF LN-1STPYDA = 14 OR 15
                 IF (NUM-DA = 28 OR 29) AND NDTE-MM = 03
                    MOVE LN-1STPYDA TO NDTE-DD.

      * TEST IF PAYDATE IS OVER GRACE PERIOD:
           MOVE NDTE-DATE TO NUM-DATE.
           MOVE LCAS-PAYDATE TO SYS-DATE.
           MOVE SP-LCYRTYPE TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO LCAS-ELAPSED-DAYS.

      * THIS ROUTINE REQUIRES NDTE-DATE IN AND RETURNS NUM-DATE:
           PERFORM LCAS-GET-GRACE-DATE.

           IF LN-CURBAL NOT = 0
              IF LCAS-ELAPSED-DAYS > LCAS-LCGRACE
                 MOVE LCAS-PAYDATE TO SYS-DATE
                 IF SYS-DATE > NUM-DATE
                    MOVE "Y" TO LCAS-ASSESS.

       LATE-ASSESS-EXIT.
           EXIT.

      *******************************************
