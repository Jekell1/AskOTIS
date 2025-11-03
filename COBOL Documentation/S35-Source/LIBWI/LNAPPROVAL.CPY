      * COPYMEMBER: LIBWI/LNAPPROVAL
      ******************************************************************
      *
      *  MEMBER NAME: LIBWI/LNAPPROVAL
      *
      *  DESCRIPTION: LOAN APPROVAL VERIFICATION & WINDOW
      *
      *  NEEDED IN  : LP/LONPB0; LP/LONPC0; LP/XONPC0; LP/ZONPC0
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 041698 ADDED CHECK FOR 'GI' ON SET A LIMIT ALONG WITH
      *            CHECKING 'GR'. OVERSIGHT ON MY PART.
      * JTG 990726 CHANGED FOR REVOLVING ACCOUNTS THE TEST AGAINST APPROVAL
      *            LIMITS TO COMPARE PD-CREDITLIM AND NOT FINANCED-AMOUNT
      * BLV 030228 PUT BACK LOGIC TO CHECK GROUP APPROVAL LIMIT IF
      *            NEW GP FLAG IS SET TO USE THIS FEATURE, WORLD PR# 315
      * BLV 030306 SAVE GR-LN-APPROVAL-LIMIT IN SAVE-GR-APPROVAL-LIMIT,
      *            THE CHECK MEMBER LOGIC IN THE PASSWORD CHECK WAS
      *            WIPING OUT THE CORRECT GROUP APPROVAL LEVEL,
      *            WORLD PR# 315
      * BLV 030327 CHANGE OVER GROUP LIMIT VERBAGE FOR DOUG JONES
      * KEC 050524 PR#2516 PREMIER ADVISORY 
      *            TO ALLOW A 'APPROVED' LOAN APP BUILT LOAN TO NOT
      *            GET STOPPED FOR ADDITIONAL APPROVAL, I ADDED:
      *               IF ( GP-I-AM-PREMIER-ADVISORY ) AND
      *                  ( LNAPPROVAL-CREATION-APP  )
      *                  GO TO EXIT-VERIFY-LOAN-APPROVAL.
      * KEC 061005 PR#2783 PREMIER ADVISORY 
      *            REVISED "VERIFY-LOAN-APPROVAL" SECTION TO BETTER
      *            MANAGE THE NEEDS OF EACH INDIVIDUAL CUSTOMERS.
      *            CHANGE FOR PREMIER TO USE LOAN AMOUNT (PD-LNAMT)
      *            INSTEAD OF FINANCED AMOUNT (FINANCED-AMOUNT).
      * BLM 160824 IF RUNNING AS WORLD, BYPASS CHECKING APPROVAL LIMIT
      *            IF LOAN IS A PB BUT NOT A PBI OR PBD (PB INCREASE OR
      *            PB DELINQUENT), WORLD PR# 1182
      * BLM 161025 FOR MEXICO, CONTINUE TO CHECK APPROVALS FOR
      *            ALL PBS, NOT JUST PBI OR PBD AS IN PR# 1182 ABOVE,
      *            WORLD PR# 1198
      * KEC 180822 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 2021.0505 CHANGE THE AMOUNT TESTED AGAINST APPROVAL LIMITS
      *               FROM AMOUNT FINANCED TO NEW PROCEEDS + PRIOR LOAN
      *               PAYOFF, #1497
      * BAH 2024.1106 CHANGED WS-PROCEEDS-AMT AND DAW-PROCEEDS-AMT
      *               TO BE TOTAL-REPAY #1684 S35Q-143
      * BAH 2024.1205 ALLOW SP-PBINCR-PERCENT VALUE OF 1 TO BE 1% FOR
      *               PBI'S, NOT WHAT IT WAS DOING (WAS TESTING FOR A
      *               PENNY GREATER THAN THE PRIOR LOAN). #1692 S35Q-151
      ******************************************************************
      *
      *    A P P R O V A L - L I M I T - W I N D O W
      *
      ******************************************************************
       APPROVAL-LIMIT-WINDOW SECTION.

           MOVE 12 TO WINDOW-LINES.
           MOVE 62 TO WINDOW-COLUMNS.
           MOVE 11 TO WINDOW-BEGIN-Y.
           MOVE 10 TO WINDOW-BEGIN-X.

           MOVE "GLIMI2" TO VDU-FORM-NAME
           MOVE "WI/GLIMI2" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY GLIMI2-SCREEN.

           MOVE GLIMI2-FORM-FIELDS TO WR-BUF.
           MOVE GLIMI2-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

      * YOUR LOGIN LAST 3 MUST HAVE A GROUP TO MATCH IT WITH AN APPROVAL LIMIT
      * HIGH ENOUGH TO APPROVE THE LOAN
           MOVE LPLOAN-USER-NAME TO USERID-GROUP-NAME.
           MOVE USER-GROUP       TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF         TO LNAPPROVAL-APPROVAL-ID.

      * GET LOAN APPROVAL LIMIT FOR VERIFICATION

           MOVE BRANCH  TO GI-BRANCH.
           MOVE LC-CODE TO GI-CLASS.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE ZEROES TO GI-LN-APPROVAL-LIMIT.

      * GET GROUP LOAN APPROVAL LIMIT FOR VERIFICATION

           MOVE LNAPPROVAL-APPROVAL-ID TO GR-ID.
           PERFORM OPEN-GR1-FILE.
           PERFORM READ-GR1-FILE.
           PERFORM CLOSE-GR1-FILE.
           IF IO-GOOD
              MOVE GR-LN-APPROVAL-LIMIT TO HOLD-GR-APPROVAL-LIMIT
           ELSE
              MOVE 0 TO HOLD-GR-APPROVAL-LIMIT.


           PERFORM SET-HOLD-FROM-PD.

      *===============================================================
       APPROVAL-WINDOW-MAIN-MODULE.

           PERFORM RESET-PD-FROM-HOLD.

           PERFORM APPROVAL-WINDOW-DISPLAY.

      *===============================================================
       SET-LIMIT-OR-EXIT.

           MOVE LEGEND-01-APPROVAL  TO LNAPPROVAL-LEGEND.
           PERFORM SEND-LNAPPROVAL-LEGEND.

           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY        ) GO TO APPROVAL-WINDOW-MAIN-MODULE.
           IF ( F7-KEY        ) GO TO END-APPROVAL-LIMIT-WINDOW.
           IF ( NOT ENTER-KEY ) GO TO SET-LIMIT-OR-EXIT.

           IF NOT ( VDUBUF = "A" OR "S" )
              GO TO SET-LIMIT-OR-EXIT.

           MOVE VDUBUF TO WS-SET-APPROVAL-FG.

      *-----------------------------------------------------------------
      *  PASSWORD NEEDED IF NOT LOGIN FOR THAT BRANCH.
      *  DON'T ASK FOR PASSWORD IF ANSWERED CORRECTLY BEFORE
      *-----------------------------------------------------------------
       GET-APPROVAL-PASSWORD.

           IF ( PASSWDW-STAT = "OK" )
              GO TO PERFORMING-SELECTED-ACTION.

           MOVE "OK" TO PASSWDW-STAT.
           PERFORM CHECK-FOR-PASSWORD.
           IF ( PASSWDW-STAT NOT = "OK" )
              GO TO SET-LIMIT-OR-EXIT.

       PERFORMING-SELECTED-ACTION.

           IF ( WS-SET-APPROVAL-APPROVED )
              GO TO SET-TO-APPROVED
           ELSE
           IF ( WS-SET-APPROVAL-SET-LIMIT )
              GO TO SET-APPROVAL-LIMIT
           ELSE
              GO TO SET-LIMIT-OR-EXIT.

       SET-TO-APPROVED.

           IF GP-GROUP-APPROVAL-FG = "Y"
              IF ( WS-TOTAL-REPAY > GI-LN-APPROVAL-LIMIT ) AND
                 ( WS-TOTAL-REPAY > HOLD-GR-APPROVAL-LIMIT )
                 MOVE "TOTAL REPAY AMT EXCEEDS YOUR APPROVAL AUTHORITY!"
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE "HIGHER AUTHORITY NEEDED TO SET THIS LIMIT"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO SET-LIMIT-OR-EXIT.

           MOVE "S"                    TO WS-APPROVAL-FG.
           MOVE WS-TOTAL-REPAY         TO PD-APPROVAL-LIMIT.
           MOVE LNAPPROVAL-APPROVAL-ID TO PD-APPROVAL-USERID.

           PERFORM APPROVAL-WINDOW-DISPLAY.
           GO TO SET-LIMIT-OR-EXIT.

      *================================================================
       SET-APPROVAL-LIMIT.

           MOVE LEGEND-02-APPROVAL TO LNAPPROVAL-LEGEND.
           PERFORM SEND-LNAPPROVAL-LEGEND.

           MOVE "E NN070010." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO APPROVAL-WINDOW-MAIN-MODULE.
           IF ( NOT ENTER-KEY ) GO TO SET-APPROVAL-LIMIT.

           IF GP-GROUP-APPROVAL-FG = "Y"
              IF ( VDUNUM0 > GI-LN-APPROVAL-LIMIT ) AND
                 ( VDUNUM0 > HOLD-GR-APPROVAL-LIMIT )
                 MOVE "TOTAL REPAY AMT EXCEEDS YOUR APPROVAL AUTHORITY!"
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE "HIGHER AUTHORITY NEEDED TO SET THIS LIMIT"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO SET-APPROVAL-LIMIT.

           MOVE VDUNUM0 TO PD-APPROVAL-LIMIT.
           MOVE "S"     TO WS-APPROVAL-FG.

           IF ( PD-APPROVAL-LIMIT < WS-TOTAL-REPAY )
              MOVE "APPROVAL LIMIT SET IS INSUFFICENT FOR UPDATING!"
                 TO MESS
              PERFORM SEND-MESS.

           MOVE LNAPPROVAL-APPROVAL-ID TO PD-APPROVAL-USERID.
           PERFORM LIMIT-WINDOW-DISPLAY.

           GO TO SET-LIMIT-OR-EXIT.

       END-APPROVAL-LIMIT-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.

           DESTROY GLIMI2-SCREEN.
           PERFORM CLOSE-WINDOW.

       EXIT-APPROVAL-LIMIT-WINDOW.
           EXIT.

      ******************************************************************
      *
      *    V E R I F Y - L O A N - A P P R O V A L                 (VLA)
      *
      *=================================================================
      *    DESC:  USED TO CHECK IF A GIVEN AMOUNT IS GREATER THAN LIMITS
      *           (GI/GR/PD).
      *
      ******************************************************************
       VERIFY-LOAN-APPROVAL SECTION.

           PERFORM LOAN-CALCULATIONS.

           MOVE ZEROES                     TO LNAPPROVAL-AMT-APP
                                              LNAPPROVAL-AMT-GI
                                              LNAPPROVAL-AMT-GR
                                              LNAPPROVAL-AMT-PD
                                              LNAPPROVAL-AMT-VERIFY.


      * GET AMOUNT FROM GI

           MOVE BRANCH                     TO GI-BRANCH.
           MOVE LC-CODE                    TO GI-CLASS.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.
           IF ( IO-BAD )
              MOVE ZEROES                  TO GI-LN-APPROVAL-LIMIT.


      * GET AMOUNT FOR GROUP

           MOVE LPLOAN-USER-NAME TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF         TO GR-ID.
           PERFORM OPEN-GR1-FILE.
           PERFORM READ-GR1-FILE.
           PERFORM CLOSE-GR1-FILE.
           IF ( IO-BAD )
              MOVE ZEROES                  TO GR-LN-APPROVAL-LIMIT.

      *****************************************************************
      * SET ALL LIMITS
      *****************************************************************
           MOVE GI-LN-APPROVAL-LIMIT       TO LNAPPROVAL-AMT-GI.
           MOVE GR-LN-APPROVAL-LIMIT       TO LNAPPROVAL-AMT-GR.
           MOVE PD-APPROVAL-LIMIT          TO LNAPPROVAL-AMT-PD.
           MOVE TOTAL-REPAY                TO LNAPPROVAL-AMT-VERIFY.

      * PRIOR-LNAMT = PRIOR LOAN PAYOFF
      *    COMPUTE LNAPPROVAL-AMT-VERIFY = PD-PROCEEDS + PRIOR-LNAMT.

      *****************************************************************


           MOVE "A" TO LNAPPROVAL-STATUS.

      *  IF PB (AND NOT PB DELINQUENT OR PB INCREASE), SKIP CHECKING
      *  APPROVAL LIMIT

           IF BR-STATE = "MX"
              GO TO CHECK-LIMIT.

           IF PD-SRCD NOT = "PB"
              GO TO CHECK-LIMIT.

           IF SP-PBDEL-DAYS NOT = 0
              IF SAVE-PRPDUE-DAYS > SP-PBDEL-DAYS
      * PBD
                 GO TO CHECK-LIMIT.

      * BAH 2024.1205 ALLOW SP-PBINCR-PERCENT VALUE OF 1 TO BE 1% FOR
      *               PBI'S, NOT WHAT IT WAS DOING (WAS TESTING FOR A
      *               PENNY GREATER THAN THE PRIOR LOAN). #1692

      * A VALUE OF 1 WILL ALLOW A LOAN INCREASE OF UP TO
      * BUT NOT INCLUDING 1% WITHOUT CHECKING BRANCH APPROVAL
      * LIMITS. ELSEWHERE THROUGH THE SYSTEM (STATS, REPORTS)
      * A PBI IS DEFINED AS ANY ACCOUNT THAT IS A PENNY OR MORE
      * GREATER THAN THE PRIOR. FOR PRE-COMPUTE ACCOUNTS
      * LOAN AMOUNT IS BASED ON TOTAL REPAYABLE, FOR SIMPLE
      * INTEREST ACCOUNTS IT IS BASED ON AMOUNT FINANCED PLUS
      * SERVICE CHARGE

           IF SP-PBINCR-PERCENT NOT = 0

      *       IF SP-PBINCR-PERCENT = 1
      *          IF PD-LNAMT > SAVE-PRLNAMT
      *             GO TO CHECK-LIMIT
      *          END-IF
      *       ELSE

      * PBI
                 COMPUTE SRCD-WORKER ROUNDED =
                     SAVE-PRLNAMT - 0.01
                         + SAVE-PRLNAMT * SP-PBINCR-PERCENT / 100
                 IF PD-LNAMT > SRCD-WORKER
                    GO TO CHECK-LIMIT.

           GO TO VLA-MESSAGES-ROUTINE.
           
       CHECK-LIMIT.

      * STANDARD BRANCH APPROVAL LIMITS
      *       LNAPPROVAL-AMT-VERIFY =  TOTAL REPAY

           IF ( GP-GROUP-APPROVAL-FG = "Y" )
              IF ( LNAPPROVAL-AMT-VERIFY > LNAPPROVAL-AMT-GI ) AND
                 ( LNAPPROVAL-AMT-VERIFY > LNAPPROVAL-AMT-GR ) AND
                 ( LNAPPROVAL-AMT-VERIFY > LNAPPROVAL-AMT-PD )
                 MOVE "U" TO LNAPPROVAL-STATUS.

           IF ( GP-GROUP-APPROVAL-FG NOT = "Y" )
              IF ( LNAPPROVAL-AMT-VERIFY > LNAPPROVAL-AMT-GI ) AND
                 ( LNAPPROVAL-AMT-VERIFY > LNAPPROVAL-AMT-PD )
                 MOVE "U" TO LNAPPROVAL-STATUS.

       VLA-MESSAGES-ROUTINE.

           IF ( LNAPPROVAL-STATUS = "U"    ) AND
              ( PD-APPROVAL-LIMIT = ZEROES )
              MOVE "TOTAL REPAY AMT REQUIRES MANAGEMENT APPROVAL"
                                  TO MESS
              PERFORM SEND-MESS
              MOVE "PLEASE SAVE THIS LOAN AND CONTACT YOUR DIRECTOR"
                                  TO MESS
              PERFORM SEND-MESS.

           IF ( LNAPPROVAL-STATUS     = "U"    ) AND
              ( PD-APPROVAL-LIMIT NOT = ZEROES )
              MOVE "APPROVAL LIMIT SET IS INSUFFICENT FOR UPDATING!"
                                  TO MESS
              PERFORM SEND-MESS
              MOVE "APPROVAL LIMIT SET IS $&&&&&&&&& " TO MESS
              MOVE PD-APPROVAL-LIMIT TO MESSAGE-NA-LIMIT
              INSPECT MESS REPLACING FIRST "&&&&&&&&&" BY
                                   MESSAGE-NA-LIMIT
              PERFORM SEND-MESS
              MOVE "PLEASE SAVE THIS LOAN AND CONTACT YOUR DIRECTOR"
                                  TO MESS
              PERFORM SEND-MESS.

       VERIFY-LOAN-APPROVAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    M I S C - P A R A G R A P H S - APPROVAL-LIMIT-WINDOW
      *
      ******************************************************************
       MISC-APPROVAL-LIMIT-WINDOW SECTION.

       APPROVAL-WINDOW-DISPLAY.

           PERFORM SETUP-LIMIT-WINDOW-BUFFER.
           PERFORM SEND-LIMIT-INFO.

       LIMIT-WINDOW-DISPLAY.

           PERFORM SETUP-LIMIT-WINDOW-BUFFER.
           PERFORM SEND-LIMIT-INFO.

       SETUP-LIMIT-WINDOW-BUFFER.

      *    COMPUTE WS-PROCEEDS-AMOUNT ROUNDED = 
      *                              (PD-PROCEEDS + PRIOR-LNAMT + .49).

           COMPUTE WS-TOTAL-REPAY ROUNDED = TOTAL-REPAY + 0.49.
           MOVE WS-TOTAL-REPAY        TO DAW-TOTAL-REPAY.
           MOVE GI-LN-APPROVAL-LIMIT  TO DAW-GLIMIT.
           MOVE PD-APPROVAL-LIMIT     TO DAW-010.
           MOVE PD-APPROVAL-USERID    TO DAW-APPROVAL-ID.

       SET-HOLD-FROM-PD.

           MOVE "X"                  TO WS-APPROVAL-FG.
           MOVE PD-APPROVAL-LIMIT    TO HOLD-APPROVAL-LIMIT.
           MOVE PD-APPROVAL-USERID   TO HOLD-APPROVAL-USERID.
           MOVE SPACES               TO PASSWDW-STAT.

       RESET-PD-FROM-HOLD.

           MOVE "X"                    TO WS-APPROVAL-FG.
           MOVE HOLD-APPROVAL-LIMIT    TO PD-APPROVAL-LIMIT.
           MOVE HOLD-APPROVAL-USERID   TO PD-APPROVAL-USERID.

       SEND-APPROVAL-INFO.
           MOVE DISPLAY-APPROVAL-WINDOW-LIST TO WR-LIST.
           MOVE DISPLAY-APPROVAL-WINDOW-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       SEND-LIMIT-INFO.

           MOVE DISPLAY-LIMIT-WINDOW-LIST TO WR-LIST.
           MOVE DISPLAY-LIMIT-WINDOW-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       SEND-LNAPPROVAL-LEGEND.
           MOVE LNAPPROVAL-LEGEND-LIST TO WR-LIST.
           MOVE LNAPPROVAL-LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.
