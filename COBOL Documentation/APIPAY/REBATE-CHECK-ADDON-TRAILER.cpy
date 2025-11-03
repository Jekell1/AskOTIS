      ********************************************************************
      * CHECK TO SEE IF THE THE INSURANCE IN QUESTION WAS PUT ON AS AN
      * ADDON OR AS PART OF THE ORIGINAL LOAN.
      * IF NO LT INS RECORDS EXIST THEN IT WAS PUT ON WITH THE LOAN ELSE IT
      * WAS ADDED, ADJUSTED, CANCELLED AFTER THE LOAN WAS BOOKED.
      * 71105#0918
      ********************************************************************
       REBATE-CHECK-ADDON-TRAILER SECTION.
      * OTHER PARTS OF REBATE EXPECT LT-REC TO BE INTACT, SAVE HERE TO NOT
      * DISRUPT OTHER USES.
           MOVE LTI-REC TO REB-HOLD-LTI-REC.
           PERFORM OPEN-LTI1-FILE.
           MOVE 0 TO LTI-SEQNO.
           MOVE "N" TO REB-LTI-ADDON-FG.
           EVALUATE REB-SUB
      * CL
               WHEN 1 MOVE 1 TO REB-TEST-LTI-TYPE
               WHEN 2 MOVE 1 TO REB-TEST-LTI-TYPE
               WHEN 3 MOVE 1 TO REB-TEST-LTI-TYPE
               WHEN 4 MOVE 1 TO REB-TEST-LTI-TYPE
      * AH
               WHEN 5 MOVE 2 TO REB-TEST-LTI-TYPE
      * PP
               WHEN 6 MOVE 3 TO REB-TEST-LTI-TYPE
               WHEN 11 MOVE 3 TO REB-TEST-LTI-TYPE
      * 01-05
               WHEN 12 MOVE 4 TO REB-TEST-LTI-TYPE
               WHEN 13 MOVE 5 TO REB-TEST-LTI-TYPE
               WHEN 14 MOVE 6 TO REB-TEST-LTI-TYPE
               WHEN 15 MOVE 7 TO REB-TEST-LTI-TYPE
               WHEN 16 MOVE 8 TO REB-TEST-LTI-TYPE
               WHEN OTHER GO TO REBATE-CHECK-ADDON-TRAILER-EXIT
           END-EVALUATE.
       REBATE-CHECK-ADDON-TRAILER-NEXT.
           MOVE LN-ACCTNO TO LTI-ACCTNO.
           ADD 1 TO LTI-SEQNO.
           PERFORM READ-LTI1-FILE.
           IF IO-BAD
              GO TO REBATE-CHECK-ADDON-TRAILER-EXIT
           ELSE
              IF LTI-INS-TYPE = REB-TEST-LTI-TYPE
                 MOVE "Y" TO REB-LTI-ADDON-FG
              END-IF
              GO TO REBATE-CHECK-ADDON-TRAILER-NEXT
           END-IF.
       REBATE-CHECK-ADDON-TRAILER-EXIT.
           PERFORM CLOSE-LTI1-FILE.
           MOVE REB-HOLD-LTI-REC TO LTI-REC.

      *-----------------------------------------------------------------------
      *    DUMMY SECTION TO INCLUDE IN COPY MEMBERS THAT NEED TO BE
      *    REFLECTED IN 'WHAT' COMMANDS FOR SUPPORT
      *-----------------------------------------------------------------------
