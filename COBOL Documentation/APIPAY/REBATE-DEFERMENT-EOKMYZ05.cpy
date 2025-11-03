      ****************************************************************
      *    DEFERMENT REBATE ROUTINE  E   (01)
      *              TENNESSEE
      *    DEFERMENT REBATE ROUTINE  O   (01)
      *              TENNESSEE                   (WITH PAY SCHEDULES)
      *    DEFERMENT REBATE ROUTINE  K   (02)
      *              INDIANA
      *    DEFERMENT REBATE ROUTINE  M   (03)
      *              CALIFORNIA
      *    DEFERMENT REBATE ROUTINE  Y   (04)
      *              S. CAROLINA                 (MERCURY)
      *    DEFERMENT REBATE ROUTINE  Z   (05)
      *              ARIZONA                     (MERCURY)
      *    DEFERMENT REBATE ROUTINE  0 5 (06)
      *              WISCONSIN                   (CITIZENS)
      ****************************************************************
       REBATE-DEFERMENT-EOKMYZ05 SECTION.
           IF REB-LN-TOTNODEF = 0
              GO TO REBATE-DEFERMENT-EOKMYZ05-EXIT.

      * CALL SUBPROGRAM TO DETERMINE REDUCTION:
           MOVE "B" TO REB-FORM-PROG-TYPE.
           IF SP-RBFRMLA(REB-SUB) = "E" OR "O"
              MOVE 01 TO REB-FORM-PROG-NO
              MOVE SP-RBFRMLA(REB-SUB) TO B01-FRMLA
              MOVE LN-PAY-SCHLD-FG TO B01-PAY-SCHLD-FG
              MOVE REB-DEF-STOP TO B01-DEF-STOP
           ELSE
           IF SP-RBFRMLA(REB-SUB) = "K"
              MOVE 02 TO REB-FORM-PROG-NO
              MOVE REB-DEF-STOP TO B02-DEF-STOP
           ELSE
           IF SP-RBFRMLA(REB-SUB) = "M"
              MOVE 03 TO REB-FORM-PROG-NO
              MOVE REB-DEF-STOP TO B01-DEF-STOP
           ELSE
           IF SP-RBFRMLA(REB-SUB) = "Y"
              MOVE 04 TO REB-FORM-PROG-NO
           ELSE
           IF SP-RBFRMLA(REB-SUB) = "Z"
              MOVE 05 TO REB-FORM-PROG-NO
           ELSE
           IF SP-RBFRMLA(REB-SUB)  = "0" AND
              SP-RBFRMLA2(REB-SUB) = "5"
              MOVE 06 TO REB-FORM-PROG-NO.

           MOVE REB-PAYDATE TO B01-PAYDATE.
           PERFORM REBATE-CALL-SUBPROG.
           IF REB-COMMON-RTNCD = " " OR "E"
              MOVE REB-COMMON-REBATE TO REB-REBATE.

       REBATE-DEFERMENT-EOKMYZ05-EXIT.
           EXIT.

      *********************************************************************
      *    ACTUARIAL REBATE ROUTINE  F-06
      *    LOGIC FOR RBFRMLA = 'F' AND '0 6'
      *
      *         TRUE ACTUARIAL REBATING ROUTINE:
      *
      * THIS ROUTINES USING THE APR OR ESR RATE
      * AND TAKES INTO ACCOUNT 1ST PAYMENT EXTENSION DAYS (+ OR - NO LIMIT)
      * 1ST PAYMENT CHARGES (ODD 1ST PAYMENTS)
      *********************************************************************
