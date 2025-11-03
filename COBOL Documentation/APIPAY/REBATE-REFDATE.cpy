                                * REB-ELAPSED-DAYS / ELAPSED-DAYS.
      *********************************
      *    REBATE REF DATE ROUTINE
      *********************************
       REBATE-REFDATE SECTION.
      * = 0 SIGNALS ERROR FOR SP-RBDATE / DATE CALC:
           MOVE 0 TO REB-REFDATE.
           MOVE SP-RBDTE(REB-SUB REB-SUB2) TO REB-HOLD-RBDTE.

      * WHEN NON MONTHLY PAPER AMD SP-RBUNITPER-CD = 'B'
      
           IF SP-RBUNITPER-CD(REB-SUB) = "B" AND
              (NOT (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1 ))
              MOVE LN-UNITPER-CD       TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ     TO DATER-UNITPER-FREQ
              MOVE REB-LN-ORIG-1STPYDATE  TO NDTE-DATE
              MOVE -1                  TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE           TO REB-REFDATE
              GO TO REBATE-REFDATE-EXIT.

           IF SP-RBEARLY(REB-SUB) = 1
              PERFORM REBATE-EP1.

           IF REB-HOLD-RBDTE = "A"
              MOVE REB-LN-LOANDATE TO REB-REFDATE.
           IF REB-HOLD-RBDTE = "B"
              MOVE REB-LN-INTDATE  TO REB-REFDATE.

      * LOGIC FOR EARNINGS CALCULATIONS:
      * WHEN 'X' USE REB-LN-INTDATE UNLESS SALES FIN. USE PURCHDATE
           IF REB-HOLD-RBDTE = "X"
              IF REB-LN-SALEFIN = "Y"
                 MOVE REB-LN-PURDATE TO REB-REFDATE
              ELSE
                 MOVE REB-LN-INTDATE TO REB-REFDATE.

           IF NOT (REB-HOLD-RBDTE = "C" OR "D")
              GO TO REBATE-REFDATE-EXIT.

      * ONE PMT INTERVAL PRIOR TO (TRIGGER) FIRST PMT DATE:
           MOVE REB-TRIGGER-DATE TO NDTE-DATE.
           MULTIPLY -1 BY 1 GIVING NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO REB-REFDATE.
       REBATE-REFDATE-EXIT.
           EXIT.

      ***************************
      * 78'S FOR ORIGINAL TERM:
      ***************************
