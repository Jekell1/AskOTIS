      *================================================================*
      * END COPYBOOK: LIBGB\DECLAR2.CPY                                *
      *================================================================*
      ******************************************************************
      *
      *  PROGRAM CONTROL
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 99 TO RETURN-STATUS.

           PERFORM GET-GPENV.

           MOVE EXT-FILPATH TO HOLD-EXT-FILPATH.
           ACCEPT ORIG-POSTNAME FROM ENVIRONMENT "POSTNAME".

      * THIS COULD CHANGE TO USE THE BP-REC WITH MORE FIELDS. I USED SIMPLEST TO
      * START OFF WITH. LOOK AT WHAT IS IN BATPAY.SH IN SH DIRECTORY

           MOVE INPUT-STRING TO BT-REC.

           MOVE BT-ACCTNO    TO BP-LNNO.
           MOVE 0            TO BP-SSNO.
           MOVE BT-AMT       TO BP-TRAMT.
           MOVE BT-REFCD     TO BP-REFCD.
           MOVE BT-PAYDATE   TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BP-PAYDATE.
           MOVE BT-TRCD      TO BP-TRCD.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE BT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE 2                TO RETURN-STATUS
              MOVE "BRANCH &&&& NOT ON FILE, ABORTED" TO LOG-MSG
              INSPECT LOG-MSG REPLACING FIRST "&&&&" BY BT-BRANCH
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"      TO CD-TYPE.
           MOVE BP-REFCD  TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE = "LC" OR "C")
              MOVE 3                TO RETURN-STATUS
              MOVE "REF CODE &&&&& NOT ON FILE, ABORTED" TO LOG-MSG
              INSPECT LOG-MSG REPLACING FIRST "&&&&&" BY BP-REFCD
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           IF CD-BR-PAYOFF-FG = "Y"
              MOVE "N" TO HOLD-BP-ALLOW-PMT-ACCT-OTHBAL
                          HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL
              MOVE "Y" TO HOLD-BP-PAYOFF-NONCASH
                          HOLD-LBOX-PAYOFF-NONCASH
           ELSE
              MOVE BR-BP-ALLOW-PMT-ACCT-OTHBAL TO
                                    HOLD-BP-ALLOW-PMT-ACCT-OTHBAL
              MOVE BR-LBOX-ALLOW-PMT-ACCT-OTHBAL TO
                                    HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL
              MOVE BR-BP-PAYOFF-NONCASH TO
                                    HOLD-BP-PAYOFF-NONCASH
              MOVE BR-LBOX-PAYOFF-NONCASH TO
                                    HOLD-LBOX-PAYOFF-NONCASH.

           MOVE CD-BR-LC-FG TO BATCH-REFCD-LC-FG.

      * ONLY PROCESS FILES FOR THE MACHINE YOU ARE ON

           IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
              MOVE 4                TO RETURN-STATUS
              MOVE "BRANCH NOT ON THIS MACHINE, ABORTED" TO LOG-MSG
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              BP-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LXE-PATH-OVERRIDE
                                              LXG-PATH-OVERRIDE.


      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE 5                       TO RETURN-STATUS
              MOVE "GLOBAL REC MISSING FOR &&&&, ABORTED" TO LOG-MSG
              INSPECT LOG-MSG REPLACING FIRST "&&&&" BY BT-BRANCH
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

      * CHANGE THE EXTERNAL FILPATH TO THAT OF THE CURRENT BRANCH.

           MOVE BR-MACHINE            TO EXT-FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO EXT-FILPATH-DATA.
           SET ENVIRONMENT "FIL"      TO EXT-FILPATH-FIL.
           MOVE ORIG-POSTNAME         TO WORK-USERID.
           MOVE BR-NO                 TO WORK-BR.
           SET ENVIRONMENT "POSTNAME" TO WORK-USERID.
           MOVE BR-NO                 TO BRANCH.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                     *
      * FROM THIS POINT FORWARD THE USER HAS BE PLACED, LOGICALLY, INTO     *
      * THE BRANCH FOR WHICH HE/SHE IS POSTING FOR.                         *
      *                                                                     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      * TEST RC-STATUS FOR OPEN DAY

           MOVE "A1"       TO RC-STATUS.
           MOVE BR-NO      TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.

           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-FG NOT = 0
              MOVE 6                       TO RETURN-STATUS
              MOVE "DAY NOT OPEN, ABORTED" TO LOG-MSG
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * DAY IS OPEN, RESERVE OP FILE SO THE DAY CANNOT BE CLOSED.

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE BR-NO       TO OPEN-PATH-BRNO.
           MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE.
           PERFORM LOAD-OPEN-FILE.
           MOVE OPEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "COULD NOT OPEN DAY" TO LOG-MSG
              MOVE 7                       TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           PERFORM OPEN-OP-FILE.

           IF (BP-PAYDATE = 20202020 OR BP-PAYDATE = 02020202
                 OR BP-PAYDATE = 0)
              MOVE TRANS-DATE TO BP-PAYDATE.

           MOVE " "      TO OVPAID-FLAG.
           MOVE 0        TO OVPAID-AMT HOLD-OVPAID-CHECK.
           MOVE BP-TRAMT TO ORIG-BP-TRAMT.
           MOVE " "      TO POSTING-ERRCD.

           IF (BR-AUTO-PL-REF-AC = "Y" OR "2" OR "3")
              IF (BP-TRCD = "PL" OR "P2" OR "P3")
                 MOVE "AUTO ACCELERATE FG:REJECT" TO LOG-MSG
                 MOVE 8                           TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * DONT ALLOW THE BACKDATE TO A PRIOR MONTH FOR PL TYPE TRANSACTION
           IF BP-TRCD =  "PL" OR "P2" OR "P3"
              MOVE BP-PAYDATE TO NUM-DATE
              MOVE TRANS-DATE TO SYS-DATE
              IF NOT (NUM-MO = S-MM AND NUM-CCYY = S-CCYY)
                 MOVE 9                              TO RETURN-STATUS
                 MOVE "BACK DATE IN PRIOR MO, ABORTED" TO LOG-MSG
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           MOVE BR-NO   TO LN-OWNBR.
           MOVE BP-LNNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE 10                             TO RETURN-STATUS
              MOVE "ACCOUNT NOT ON FILE, ABORTED" TO LOG-MSG
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           IF LN-ESCROW-FG = "Y"
              MOVE "ACCOUNT HAS ESCROW"  TO LOG-MSG
              MOVE 11                    TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           IF BP-TRCD = "RV"
              PERFORM VALIDATE-REVERSAL
              IF VALID-REVERSAL-FG NOT = "Y"
                 GO TO END-ROUTINE.

           IF LN-BNKRPTDATE NOT = 0
              IF BP-TRCD = "BK"
                 MOVE "BK: ACCT ALREADY BANKRUPT" TO LOG-MSG
                 MOVE 12                          TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF LN-BNKRPTDATE = 0
              IF BP-TRCD = "BD"
                 MOVE "BD: ACCT NOT BANKRPT STAT" TO LOG-MSG
                 MOVE 13                          TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF BP-TRCD =  "PL" OR "P2" OR "P3"
              IF LN-PLCD = "P"
                 MOVE "ACCT ALREADY ACTIVE P&L" TO LOG-MSG
                 MOVE 14                        TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF BP-TRCD = "RE"
               IF GP-REPO-CODE NOT = "A"
                  MOVE "MUST HAVE REPO OPTIONS TURNED ON" TO
                                                          LOG-MSG
                  MOVE 15                            TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

           IF BP-TRCD = "RE"
               IF NOT (LN-REPOCD = "X" OR "S")
                  MOVE "RE: ACCT NOT REPO STATUS!" TO LOG-MSG
                  MOVE 16                          TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

           IF BP-TRCD = "RE"
              MOVE BP-PAYDATE TO IBPC-DATE
              PERFORM IBPC-TEST
              IF NOT (IBPC-FG = "I" OR LN-LOANTYPE = "I")
                 MOVE "RE: ACCT NOT INTEREST BEARING!" TO LOG-MSG
                 MOVE 17                             TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * "R" REJECT

           IF HOLD-CD-BR-POST-SS-TRCD = "R"
               IF LN-REPOCD = "X"
                  MOVE "REPO BATCH RECORD"   TO LOG-MSG
                  MOVE 18                    TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

           IF HOLD-CD-BR-POST-SS-TRCD = "Y"
               IF LN-REPOCD NOT = "X"
                  MOVE "NON REPO BATCH RECORD!"  TO LOG-MSG
                  MOVE 19                        TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

      * SAVE ORIG VALUES

           MOVE LN-LBOX    TO ORIG-LN-LBOX.
           MOVE LN-ALLOTCD TO ORIG-LN-ALLOTCD.
           MOVE LN-OT2BAL  TO ORIG-LN-OT2BAL.

      * SETUP LBOX/ALLOTCD HERE FOR CORRECT LATE CHARGE CALCULATIONS

           MOVE "N"        TO LN-LBOX.
           MOVE "Y"        TO LN-ALLOTCD.

      * CLEAR HOLD-BP-TRCD HERE:
      *  - IF AN EXCEPTION CAUSING A NON CASH TRASACTION REQUIREMENT
      *       IS FOUND AND BR-????-PAYOFF-NONCASH = "Y", THAN "99"
      *       WILL BE PUT INTO HOLD-BP-TRCD TO INDICATE A CALL TO
      *       LONPFA.C SHOULD BE DONE AND NOT A CALL TO LONPFC.C

           MOVE " " TO HOLD-BP-TRCD.

           MOVE BP-REFCD TO LP-REFNO.

      * ADDED ABILITY TO SEND NEGATIVES, IF 1ST BYTE OF AMT  IS "-",
      * BUT BP-ALLOW-NEGATIVE FLAG MUST ALSO BE Y, NEEDED FOR RE'S
      * (REGACC PR#382). ERROR IF NEG BUT FLAG NOT SET:

           IF BP-TRAMT < 0
              IF BP-ALLOW-NEGATIVE NOT = "Y"
                 MOVE "NEGATIVE BUT FLAG NOT SET" TO LOG-MSG
                 MOVE 20                          TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST FOR PAYMENT AMOUNT = 0.00

           IF NOT (CD-BR-FILE-TYPE = "B" OR "P")
              IF BP-ALLOW-NEGATIVE NOT = "Y"
                 IF BP-TRAMT NOT > 0
                    IF (BP-TRCD = "DF" OR "D2" OR "D3" OR "D4"
                               OR "D5" OR "D6" OR "D7" OR "D8" OR "D9")
                        IF BP-ALLOW-ZERO-DF NOT = "Y"
                           MOVE "PAYMENT MUST BE > 0" TO LOG-MSG
                           MOVE 21                    TO RETURN-STATUS
                           PERFORM CREATE-LOG
                           GO TO END-ROUTINE
                        END-IF
                    ELSE
                        MOVE "PAYMENT MUST BE > 0" TO LOG-MSG
                        MOVE 21                    TO RETURN-STATUS
                        PERFORM CREATE-LOG
                        GO TO END-ROUTINE.

           IF HOLD-CD-BR-POST-SS-TRCD = "Y"
              IF BP-AUCTION-FEES = ZEROS
                 MOVE "MISSING AUCTION FEES" TO LOG-MSG
                 MOVE 22                     TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF ODD PAYMENTS ARE NOT ALLOWED:
      * CHECK BR-BP-ODDPAY (NOT LBOX) IF ALLOTMENT

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-ODDPAY NOT = "Y"  )
              )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-ODDPAY NOT = "Y")
              )
              IF LN-1STPYAMT NOT = 0
                 IF LN-TOTPAYMNTD = 0
                    IF BP-TRAMT NOT = LN-1STPYAMT
                       MOVE "IRREGULAR PAYMENT AMOUNT" TO LOG-MSG
                       MOVE 23                       TO RETURN-STATUS
                       PERFORM CREATE-LOG
                       GO TO END-ROUTINE
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF BP-TRAMT NOT = LN-REGPYAMT
                       MOVE "IRREGULAR PAYMENT AMOUNT" TO LOG-MSG
                       MOVE 23                         TO RETURN-STATUS
                       PERFORM CREATE-LOG
                       GO TO END-ROUTINE
                    ELSE
                       NEXT SENTENCE
              ELSE
                 IF BP-TRAMT NOT = LN-REGPYAMT
                    MOVE "IRREGULAR PAYMENT AMOUNT" TO LOG-MSG
                    MOVE 23                         TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.
    
      * WHEN BR-BP/LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y", ALLOW THE BATCH
      * PAYMENT TO BE APPLIED TO THE ACCOUNT BUT NOT TO OTHER BALANCE

      * WHEN CD-BR-OT-GL-ACCT NOT = 0, ALLOW POSTING TO ACCOUNTS WITH
      * LN-OTHBAL, OTHERWISE, TEST IF OTHER CHARGES EXIST 

           IF ( CD-BR-OT-GL-ACCOUNTS = " " ) OR
              ( CD-BR-OT-GL-ACCT = 0       )
              IF LN-OTHBAL NOT = 0
                 IF ( ( CD-BR-ALLOT-OPTION = "Y"              ) AND
                      ( HOLD-BP-ALLOW-PMT-ACCT-OTHBAL NOT = "Y" )
                    )
                    OR
                    (
                      ( CD-BR-LBOX-OPTION = "Y"                 ) AND
                      ( HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL NOT = "Y" )
                    )
                    MOVE "LOAN HAS OTHER CHARGES" TO LOG-MSG
                    MOVE 24                       TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.


           IF LN-OT2BAL NOT = 0
              IF ( ( CD-BR-ALLOT-OPTION = "Y"              ) AND
                   ( HOLD-BP-ALLOW-PMT-ACCT-OTHBAL = "2" )
                 )
                 OR
                 (
                   ( CD-BR-LBOX-OPTION = "Y"                 ) AND
                   ( HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL = "2" )
                 )
                 MOVE "LOAN HAS OTHER 2 CHARGES" TO LOG-MSG
                 MOVE 25                         TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF ACCOUNT BELONGS TO POSTING BRANCH:

           IF LN-OWNBR NOT = BR-NO
              MOVE "ACCOUNT OWNED BY OTHER BR" TO LOG-MSG
              MOVE 26                         TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           IF BP-FROZEN-OVERRIDE NOT = "Y"
              IF LN-ACCT-FROZEN = "Y"
                 MOVE "ACCOUNT IS FROZEN" TO LOG-MSG
                 MOVE 27                  TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL ONLY COVER LC AND INTEREST:
      * CHECK BR-BP-NOPRIN (NOT LBOX) IF ALLOTMENT

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-NOPRIN NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-NOPRIN NOT = "Y") )
                 COMPUTE TEST-AMT = LN-INTBAL + LN-LCBAL
                 IF BP-TRAMT NOT > TEST-AMT
                    MOVE "APPLIES WITH NO PRINCIPAL" TO 
                                                  LOG-MSG
                    MOVE 28                    TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PL ACCOUNTS ARE ALLOWED:

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                ( NOT (BR-BP-PL = "Y" OR "P") ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                ( NOT (BR-LBOX-PL = "Y" OR "P") ) )
                 IF LN-PLDATE NOT = 0
                    MOVE "P&L ACCOUNT " TO LOG-MSG
                    MOVE 29             TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

           IF LN-PLCD = "I"
              MOVE "INACTIVE P&L ACCOUNT " TO LOG-MSG
              MOVE 30                      TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * TEST IF JD ACCOUNTS ARE ALLOWED:
      * CHECK BR-BP-JD (NOT LBOX) IF ALLOTMENT

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-JD NOT = "Y"      ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-JD NOT = "Y"    ) )
              IF LN-JDDATE NOT = 0
                 MOVE "JUDGEMENT ACCOUNT " TO LOG-MSG
                 MOVE 31                   TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF ACCOUNTS WITH MESSAGE ARE ALLOWED:
      * CHECK BR-BP-FLASH (NOT LBOX) IF ALLOTMENT

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-FLASH NOT = "Y"   ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-FLASH NOT = "Y" ) )
              IF LN-MESSAGE NOT = " "
                 MOVE "FLASHING MESSAGE  " TO LOG-MSG
                 MOVE 32                   TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF ACCOUNTS WITH ACTION CODE ARE ALLOWED:

           IF ( (CD-BR-ALLOT-OPTION = "Y"  ) AND
                (BR-BP-ACTIONCD NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y"   ) AND
                (BR-LBOX-ACTIONCD NOT = "Y") )
              IF LN-ACTIONCD NOT = " "
                 MOVE "ACTION CODE ON ACCOUNT" TO LOG-MSG
                 MOVE 33                       TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           PERFORM GET-SPR.
           IF IO-FG NOT = 0
              MOVE "INVALID SPR RECORD" TO LOG-MSG
              MOVE 34                   TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * REJECT IF THE INTEREST PAID THRU ON AN IB LOAN IS GREATER
      * THAN THE POSTING DATE

           MOVE BP-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           IF (IBPC-FG = "I" OR LN-LOANTYPE = "I")
               IF LN-INTPDTH-DATE > BP-PAYDATE
                  MOVE "INTEREST PAID THRU DATE" TO LOG-MSG
                  MOVE 35                        TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

           IF BP-TRCD = "DF" OR "D2" OR "D3" OR "D4" OR "D5"
                             OR "D6" OR "D7" OR "D8" OR "D9"
              MOVE "Y" TO DF-TEST
           ELSE
              MOVE " " TO DF-TEST.

           IF DF-POSTING
              IF BP-TRAMT < SP-DEFMIN OR
                      BP-TRAMT > SP-DEFMAX
                 MOVE "SPR DEFERMENT MIN/MAX" TO LOG-MSG
                 MOVE 36                      TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * ONLY ALLOWING REGIONAL'S SP-DEFFRMLA 0,2,3,96,97,99

           IF DF-POSTING
              IF NOT (SP-DEFFRMLA = 0 OR 2 OR 3 OR 96 OR 97 OR 99)
                 MOVE "DF FRMLA INVALID FOR BATCH" TO LOG-MSG
                 MOVE 37                         TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * NO MULTIPLES WITH 96 OR D7
           IF DF-POSTING
              IF SP-DEFFRMLA = 96 OR 97
                 IF BP-TRCD NOT = "DF"
                    MOVE "MULTIPLE DF'S NOT ALLOWED" TO LOG-MSG
                    MOVE 38                        TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

           IF DF-POSTING
             IF BP-IGNORE-DEFPOL NOT = "Y"
                MOVE BP-TRCD    TO DEFPOL-TRCD
                MOVE TRANS-DATE TO DEFPOL-DATE
                PERFORM VALIDATE-DF-POLICY
                IF DEFPOL-MAXIMUM = "X"
                   MOVE "DEFERMENTS WILL EXCEED ORIGINAL TERM" TO
                                                      LOG-MSG
                   MOVE 39                        TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE
                ELSE
                IF DEFPOL-MAXIMUM = "R"
                   MOVE "MINIMUM MONTHS HAVE NOT ELAPSED" TO 
                                                  LOG-MSG
                   MOVE 40                    TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE
                ELSE
                IF DEFPOL-MAXIMUM = "Y"
                   MOVE "DF NOT ALLOWED" TO LOG-MSG
                   MOVE 41               TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

      * TEST IF MAXIMUM CONTRACTUAL IS EXCEEDED:

           MOVE BP-PAYDATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

            IF BP-TRCD = "PL" OR "P2" OR "P3"
               IF CONTRACTUAL < 30
                   MOVE "CONTRACTUAL < 30" TO LOG-MSG
                   MOVE 42                 TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

            IF BP-TRCD = "PL" OR "P2" OR "P3"
               IF RECENCY < 30
                   MOVE "RECENCY < 30" TO LOG-MSG
                   MOVE 43             TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

           IF CD-BR-ALLOT-OPTION = "Y"
              IF BR-BP-MAXCON NOT = 999
                IF CONTRACTUAL > BR-BP-MAXCON
                   MOVE "EXCEEDS MAX CONTRACTUAL" TO LOG-MSG
                   MOVE 44                        TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

           IF CD-BR-LBOX-OPTION = "Y"
              IF BR-LBOX-MAXCON NOT = 999
                 IF CONTRACTUAL > BR-LBOX-MAXCON
                    MOVE "EXCEEDS MAX CONTRACTUAL" TO LOG-MSG
                    MOVE 44                        TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF MAXIMUM RECENCY IS EXCEEDED:

           IF CD-BR-ALLOT-OPTION = "Y"
              IF BR-BP-MAXREC NOT = 999
                IF RECDEL > BR-BP-MAXREC
                   MOVE "EXCEEDS MAX RECENCY" TO LOG-MSG
                   MOVE 45                    TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

              IF CD-BR-LBOX-OPTION = "Y"
                 IF BR-LBOX-MAXREC NOT = 999
                    IF RECDEL > BR-LBOX-MAXREC
                       MOVE "EXCEEDS MAX RECENCY" TO LOG-MSG
                       MOVE 45                   TO RETURN-STATUS
                       PERFORM CREATE-LOG
                       GO TO END-ROUTINE.

      * DON'T ALLOW POSTING WHEN LOAN HAS ZERO BALANCE
      * THIS CHECK WAS DOWN AFTER ALL THE PAYOFF STUFF,
      * NOT SURE WHY, I LEFT IT THERE BUT WORLD WANTS TO SEE ZERO
      * BALANCE MESSAGE INSTEAD OF PMT EXCEED PAYOFF MESSAGE, SO
      * YOU CAN LOOK AT EXCEPTION REPORT & SEE PAID OUTS VS.
      * LOANS WITH BALANCE TOO LOW FOR BATCH PMT.  BARB

           IF LN-CURBAL = 0
              IF CD-BR-ALLOT-OPTION = "Y"
                 IF HOLD-BP-PAYOFF-NONCASH NOT = "Y"
                    MOVE "ALREADY ZERO BALANCE" TO LOG-MSG
                     MOVE 46                    TO RETURN-STATUS
                     PERFORM CREATE-LOG
                     GO TO END-ROUTINE.

           IF LN-CURBAL = 0
              IF CD-BR-LBOX-OPTION = "Y"
                 IF HOLD-LBOX-PAYOFF-NONCASH NOT = "Y"
                    MOVE "ALREADY ZERO BALANCE" TO LOG-MSG
                    MOVE 46                     TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

           IF CD-BR-FILE-TYPE = "B" OR "P"
              GO TO SKIP-PAYOFF-CALC.

           IF VALID-REVERSAL-FG = "Y" OR BP-TRCD = "OT"
              GO TO SKIP-PAYOFF-CALC.

      * SET REB-LPTRCD FOR LPPOFF WHICH CALLS REBATE
      * NEED TO KNOW 'PB', 'RN', AND 'SC'
      * FOR SUN FINANCE SPOPT1 = 2:

           MOVE "PO"              TO REB-LPTRCD
                                     POFF-LPTRCD.
           MOVE BP-PAYDATE        TO POFF-PAYDATE.
           MOVE LN-MAKERCD(1)     TO POFF-MAKERCD.
           MOVE BATCH-REFCD       TO POFF-LCAP-BATCH-REFCD.
           MOVE BATCH-REFCD-LC-FG TO POFF-LCAP-BATCH-REFCD-LC-FG.
           PERFORM PAYOFF-LOAN-ROUTINE.

      * COMPILE TOTAL PAYOFF REBATES FOR FURTHER TESTING:

           MOVE 0 TO TOT-POFF-REBATES.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              ADD POFF-REBATE(SUB) TO TOT-POFF-REBATES
           END-PERFORM.

      * DETERMINE NET PAYOFF PRINCIPAL:
           COMPUTE NET-POFF-PRIN =
              LN-CURBAL - LN-OT2BAL + LN-OTHBAL - TOT-POFF-REBATES.

      * DETERMINE AMOUNT THAT WILL PAYOFF ACCOUNT:

           COMPUTE WILL-POFF-ACCOUNT =
              NET-POFF-PRIN + POFF-INTDUE + POFF-LCDUE.

      * DETERMINE TRAMT THAT CAN BE POSTED LEAVING $0.01 IN PRINCIPAL:

           COMPUTE VALID-POSTING-NOPOFF = WILL-POFF-ACCOUNT - 0.01.

      * TEST IF PAYMENT WILL BRING BALANCE BELOW A NET PAYOFF:

           IF BP-TRAMT > POFF-NETDUE
              IF LN-PLDATE NOT = 0
                 IF ( CD-BR-ALLOT-OPTION = "Y" AND
                      BR-BP-PL = "P" )
                    OR
                    ( CD-BR-LBOX-OPTION = "Y" AND
                      BR-LBOX-PL = "P" )
                    MOVE "PAY EXCEEDS PAYOFF AMT" TO LOG-MSG
                    MOVE 47                      TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL BRING BALANCE BELOW THE CURRENT LOAN
      * BALANCE

           IF BP-TRAMT > (LN-CURBAL - LN-OT2BAL) - .01
              IF LN-PLDATE NOT = 0
                 IF ( CD-BR-ALLOT-OPTION = "Y" AND
                      BR-BP-PL = "P" )
                    OR
                    ( CD-BR-LBOX-OPTION = "Y" AND
                      BR-LBOX-PL = "P" )
                    MOVE "PAY EXCEEDS PRINCIPAL " TO LOG-MSG
                    MOVE 48                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

           IF BP-TRAMT > POFF-NETDUE
              IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                   (HOLD-BP-PAYOFF-NONCASH = "Y"  ) )
                 OR
                 ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                   (HOLD-LBOX-PAYOFF-NONCASH = "Y") )
                 MOVE "99" TO HOLD-BP-TRCD
              ELSE
                 MOVE "PAY EXCEEDS PAYOFF AMT" TO LOG-MSG
                 MOVE 47                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF GP-DONT-COLL-INT-ON-SS = "Y"
              IF BP-TRCD = "SS"
                 IF BP-TRAMT > (LN-CURBAL -  .01)                  
                    MOVE "AMT > BALANCE - .01"   TO LOG-MSG
                    MOVE 49                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL PAYOFF LOAN PREMATURLY:
      *
      * N O T E:   THIS TEST IS NOT CORRECT, IT DOESN'T CONSIDER
      *            LN-LASTPYAMT OR PAYMENT SCHEDULES
      * 7/14/08 OK, I UNDERSTAND BP-PAYOFF-NONCASH DOESN'T ALLOW
      *         BATCH PAYOFF IF OTHER CHARGES ON ACCT, OR PAYMENT
      *         EXCEEDS PAYOFF, OR PAYOFF PRIOR TO MATURITY.
      *         DON'T GET WHY THIS FLAG IS CALL GP-PAYOFF-NONCASH.
      *         ANYWAY, WORLD ACCTS WILL BE PAID OFF EARLY WITH
      *         INSURANCE CLAIMS, SO ALLOW EARLY PAYOFF IF BATCH
      *         PMT = PAYOFF.  WORLD PR# 573.  BARB

           MOVE "N" TO PAYOFF-TRANS-FG.
           IF BP-TRAMT = POFF-NETDUE
              MOVE "Y" TO PAYOFF-TRANS-FG
              MOVE "99" TO HOLD-BP-TRCD
              GO TO SKIP-EARLY-PAYOFF-TEST.

           IF BP-TRAMT = POFF-NETDUE
              MOVE "Y" TO PAYOFF-TRANS-FG
              IF BP-TRAMT NOT = LN-REGPYAMT
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                    )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                    )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "ACCT WILL PAYOFF EARLY" TO LOG-MSG
                    MOVE 50                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * WHEN BR-BP/LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y"
      *      THAN ALLOW THE BATCH PAYMENT TO BE APPLIED
      *      TO THE ACCOUNT BUT NOT TO OTHER BALANCE
      *
      * 4/12/07  WE THINK THE CHECK BELOW MAKES SENSE IF THERE IS
      *          AN OTHER BAL ON THE ACCOUNT.  IF THERE IS NO OTHER
      *          BAL, THIS STOPS A PAYOFF FROM POSTING AND GIVE
      *          STUPID CUTOFF MESSAGE.   SO WE WILL ONLY DO THIS
      *          CHECK IF THERE IS AN OTHER BAL.  BARB & CINDY
      *

       SKIP-EARLY-PAYOFF-TEST.
           IF LN-OTHBAL = 0
              GO TO TEST-PAYOFF-ALLOWED.

           IF ( ( CD-BR-ALLOT-OPTION = "Y"          ) AND
                ( HOLD-BP-ALLOW-PMT-ACCT-OTHBAL = "Y" ) )
              OR
              ( ( CD-BR-LBOX-OPTION = "Y"             ) AND
                ( HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y" ) )
              IF BP-TRAMT > (VALID-POSTING-NOPOFF - LN-OTHBAL)
                 MOVE "AMT > POFF - .01 WITH OTH" TO LOG-MSG
                 MOVE 51                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF PAYOFF IS ALLOWED:

       TEST-PAYOFF-ALLOWED.
           IF ( (CD-BR-ALLOT-OPTION = "Y"  ) AND
                (BR-BP-NOPAYOFF NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y"   ) AND
                (BR-LBOX-NOPAYOFF NOT = "Y") )
              IF BP-TRAMT = POFF-NETDUE
                 MOVE "ACCOUNT WILL PAYOFF" TO LOG-MSG
                 MOVE 52                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-LC NOT = "Y"      ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-LC NOT = "Y"    ) )
              MOVE BP-PAYDATE TO IBPC-DATE
              PERFORM IBPC-TEST
              IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                                        AND (LN-JDDATE = 0)
                                         AND (LN-ACCELDATE = 0)
                 MOVE BP-TRAMT     TO LCAP-TRAMT
                 MOVE BP-PAYDATE   TO LCAP-PAYDATE
                 MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                 MOVE "PY"         TO LCAP-LPTRCD
                 MOVE 0            TO LCAP-LPAPLC LCAP-LPAPINT
                 MOVE CD-CODE      TO LCAP-BATCH-REFCD
                 MOVE CD-BR-LC-FG  TO LCAP-BATCH-REFCD-LC-FG
                 PERFORM LATE-CHARGE-APPLY
                 COMPUTE TEST-AMT  = LCAP-APP + LCAP-OWE - LN-LCBAL
                 IF TEST-AMT NOT = 0
                    MOVE "LATE CHARGE IS REQUIRED" TO LOG-MSG
                    MOVE 53                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL ONLY COVER LC AND INTEREST:

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-NOPRIN NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-NOPRIN NOT = "Y") )
              COMPUTE TEST-AMT = POFF-INTDUE + POFF-LCDUE
              IF BP-TRAMT NOT > TEST-AMT
                 MOVE "PAY APPLIES WITH NO PRIN" TO LOG-MSG
                 MOVE 28                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL CAUSE NEGATIVE CURRENT BALANCE:
      *
      * WHEN OT2BAL EXISTS, INSURE AT LEAST ($0.01 + LN-OT2BAL)
      * REMAINS IN LN-CURBAL:

           COMPUTE TEST-AMT = BP-TRAMT - POFF-INTDUE - POFF-LCDUE.
           IF LN-OT2BAL NOT = 0
              IF TEST-AMT >= (LN-CURBAL - TOT-POFF-REBATES)
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  ) )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y") )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PAY EXCEEDS OTHER 2 BAL." TO LOG-MSG
                    MOVE 55                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE
                 END-IF
              END-IF
           ELSE
      * WHEN NO OT2BAL, DON'T LET LN-CURBAL GO NEGATIVE:
              IF TEST-AMT > LN-CURBAL
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                    )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                    )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PAY EXCEEDS CURBAL" TO LOG-MSG
                    MOVE 56                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * 5/4/12:  IF THERE'S AN O2 BALANCE, IF IT'S NOT ENOUGH TO
      *          PAYOFF, BUT MORE THAN (CURBAL - O2BAL), THEN
      *          REJECT TO HANDLE MANUALLY.  FOR EXAMPLE, LN-CURBAL=70,
      *          20 OF THAT IS LN-OT2BAL.  BP-TRAMT = 50, REBATES = 15.
      *          SO TEST-AMT (50) IS LESS THAN CURBAL-REBATES (55),
      *          BUT > OR = THE BALANCE LESS O2 (50).  THE PROBLEM
      *          HERE IS THERE'S NOT ENOUGH TO PAYOFF(WOULD NEED 55),
      *          BUT WOULD NEED TO APPLY SOME OR ALL TO O2 BALANCE,
      *          AND STILL LEAVE AT LEAST 1 CENT IN BALANCE.  BETTER
      *          TO REQUIRE MANUAL POSTING.  PL 771.    BARB & CINDY

           IF LN-OT2BAL NOT = 0
              COMPUTE TEST-AMT = BP-TRAMT - POFF-INTDUE - POFF-LCDUE
              IF TEST-AMT < (LN-CURBAL - TOT-POFF-REBATES)
                 IF TEST-AMT >= (LN-CURBAL - LN-OT2BAL)
                    MOVE "REQUIRES MANUAL Z2" TO LOG-MSG
                    MOVE 57                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

       SKIP-PAYOFF-CALC.

      * DON'T ALLOW POSTING WHEN LOAN HAS ZERO BALANCE

           IF LN-CURBAL = 0
              MOVE "ALREADY ZERO BALANCE" TO LOG-MSG
              MOVE 46                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * TEST FOR VALID GENERAL LEDGER INTERFACE RECORD:

           MOVE LN-OWNBR TO GI-BRANCH.
           MOVE LN-CLASS TO GI-CLASS.
           PERFORM READ-GI1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING G/L INTERFACE " TO LOG-MSG
              MOVE 58                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           MOVE " " TO POSTING-ERRCD.

           IF CD-BR-FILE-TYPE = "B"
              PERFORM BANKRUPT-POSTING
           ELSE
           IF BP-TRCD = "RV"
              PERFORM REVERSAL-POSTING
           ELSE
           IF BP-TRCD = "OT"
              PERFORM OTHER-CHG-POSTING
           ELSE
              IF (BP-TRAMT > VALID-POSTING-NOPOFF) AND
                 (HOLD-BP-TRCD = "99"            )
                 PERFORM PAYOFF-POSTING
              ELSE
                 PERFORM PAYMENT-POSTING.

           IF NOT (POSTING-ERRCD = " " OR "O")
              MOVE "GENERIC POSTING ERROR" TO LOG-MSG
              MOVE 59                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.
            
           MOVE 0                   TO RETURN-STATUS.

           MOVE "SUCCESSFUL UPDATE" TO LOG-MSG
           PERFORM CREATE-LOG.

           PERFORM CLOSE-OP-FILE.

       END-ROUTINE.
           MOVE TEMP-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           PERFORM CLOSE-OP-FILE.

           GO TO END-PROGRAM.

      ************************************
