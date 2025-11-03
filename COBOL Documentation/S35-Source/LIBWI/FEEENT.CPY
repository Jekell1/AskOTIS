      * COPYMEMBER: LIBWI/FEEENT
      **************************************************************************
      *                  (LIBWI/FEEENT)
      *     FEE CODE ENTRY WINDOW
      *REV:
      * JTG 030595 REVISED FOR DIRECT LOANS, RE: FEE OURS
      * JFF 020896 REVISED TO ALLOW CURSOR TO START AT
      *            FEE CODE COLUMN.
      * BAH 072696 WAS LEAVING SPACES IN FEEOURS, IF YOU HIT THE
      *            UPARROW, WAS MOVING BR-OUR-FEE TO VDUBUF BUT NOT
      *            INTO THE FW1-FEEOURS FIELD !!
      * JTG 090497 CHANGED FROM 'LIBWI/FEEENT1' --> 'LIBWI/FEEENT'
      *            CHANGED ALL FIELDS 'FW1-' --> 'FW-'
      *            ADDED SET OF FW-FEEFINCHG WORLD A90
      * JTG 102397 STOPPED DIRECT LOAN FEE 'M - MEMO' SINCE
      *            LPEOD5TR, DIRECT LOAN REGISTER, CONTRACTS DON'T TEST
      * JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      *
      * KEC 020621 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED LOGIC FOR USING 'CD-GC-FEE-MAXAMT'
      *  CS 050406 ADDED TEST ON SP-INSURANCE-WRITTEN, IF 'N' AND DIRECT LOAN
      *            FREE UP ALL 10 FEED CODES
      *  CS 110111 CUSTOMER WAS ABLE TO ENTER A FEE CODE FOR AN AMOUNT WHEN
      *            FEE CODE FILE HAD A MAX OF ZERO. I WAS ABLE TO DUPLICATE IN
      *            INPUT AS WELL AS SAVED LOAN BY PRESSING F7 AT THE CODE PROMPT
      *            OR IF SITTING AT AMOUNT, PRESSING F4/THEN F7.  ADDED A RESET
      *            ON FEE LINE ELEMENT IF FEE AMOUNT > THAN CD-GC-FEE-MAXAMT
      *                                            REGENCY PL#753
      *  CS 150922 CHANGED CD-GC-BRANCH-COMM-AMOUNT TO COMP-3, TEST FOR 202020
      *                                              EXCEL PR#4821
      *
      * KEC 2016.0314 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      * BAH 190122 SPLIT CDFILE, ADDED NEW CDBFILE FOR 'GC' 
      *
      **************************************************************************
       FEEENT-WIN SECTION.
           MOVE 20 TO WINDOW-LINES.
           MOVE 40 TO WINDOW-COLUMNS.
           MOVE 3 TO WINDOW-BEGIN-Y.
           MOVE 40 TO WINDOW-BEGIN-X.
           MOVE "FEEENT" TO VDU-FORM-NAME.
           MOVE "WI/FEEENT" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY FEEENT-SCREEN.

           MOVE FEEENT-FORM-FIELDS TO WR-BUF.
           MOVE FEEENT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A160TITLE." TO VDU.
           MOVE " FEE CODE ENTRY " TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       FEEENT-WIN-RESET.
           PERFORM FEEENT-WIN-SET-WORKFEES.

           PERFORM FEEENT-WIN-LINE-DISPLAY
               VARYING SUB FROM 1 BY 1
                 UNTIL SUB > 10.

           PERFORM FEEENT-WIN-RUNNING-TOTAL.

           MOVE 3 TO FW-FIRST-FIELD.
           IF FW-PROTECT-FEE2-FG = " "
              MOVE 2 TO FW-FIRST-FIELD.

           IF FW-PROTECT-FEE1-FG = " "
              MOVE 1 TO FW-FIRST-FIELD.

      ******************************************************************
      *
      *    SELECT FIELD FROM PAGE
      *
      ******************************************************************
       FEEENT-BEGIN-ENTRY.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO FW-ONE.
           MOVE 0 TO FW-ELE.
           COMPUTE FW-ELE = FW-FIRST-FIELD - 1.
      
       FEEENT-CREATE-ROUTINE.
           ADD FW-ONE TO FW-ELE.
           IF FW-ELE < 1
              MOVE 1 TO FW-ELE.
           IF FW-ELE > FW-MAX
              MOVE FW-MAX TO FW-ELE.

      * WHEN A BRACH IS FLAGED FOR INS. SALES TAX
      *    VIA BR-FORCE-VALID-CITY-COUNTY = "Y"
      * ONLY 4 FEE CODES ARE AVAILABLE FOR THE BRANCH
      
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              IF SP-INSURANCE-WRITTEN = " " OR "Y"
                IF LC-SALEFIN NOT  = "Y"
                  IF FW-ELE > 4
                 MOVE 4 TO FW-ELE.

           IF FW-ELE NOT > FW-MAX
              GO TO FEEENT-ENTER-ELE.

       FEEENT-ENTER-ELE.
           MOVE "F7-DONE, F6-SCAN, ENTER FEE CODE:" TO MESS.
           PERFORM SEND-WSEL-LEGEND.

           MOVE FEESPEC(FW-ELE) TO VDU.
           MOVE FW-ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

       FEEENT-CHECK-ELE.

      * TEST F1 RESET:
           IF VDUSTATUS = "10"
              GO TO FEEENT-WIN-RESET.

      * TEST UP ARROW AND PROTECTED FEES:
           IF VDUSTATUS = "1U"
              IF FW-ELE = FW-FIRST-FIELD
                 GO TO FEEENT-ENTER-ELE
              ELSE
                 MOVE -1 TO FW-ONE
                 GO TO FEEENT-CREATE-ROUTINE.

      * TEST DOWN ARROW:
           IF ( VDUSTATUS = "1D" )
              IF ( FW-FEECODE(FW-ELE) NOT = ZEROES ) AND
                 ( FW-FEECODE(FW-ELE) NOT = SPACES ) AND
                 ( FW-ELE < 10 )
                 MOVE 1 TO FW-ONE
                 GO TO FEEENT-CREATE-ROUTINE
              ELSE
                 GO TO FEEENT-ENTER-ELE.

      * TEST FEE SCAN:
           IF VDUSTATUS = "1<"
              MOVE 2 TO CDCODE-FEE-FG
              PERFORM CODE-SCAN
              MOVE "00" TO VDUSTATUS
              IF IO-FG NOT = 0
                 GO TO FEEENT-ENTER-ELE
              ELSE
                 GO TO FW-GOTO-FIELD.

      * TEST F7:
           IF VDUSTATUS = "1>"
              GO TO FEEENT-WIN-EXIT.


           IF VDUBUF = " "
              PERFORM FEEENT-WIN-PURGE
              GO TO FEEENT-ENTER-ELE.

      * TEST SELECTED FIELD:

           IF VDUSTATUS NOT = "00"
              GO TO FEEENT-ENTER-ELE.

           MOVE 1 TO FW-ONE.

      *-----------------------------------------------------------------
       FW-GOTO-FIELD.
           GO TO FW-F-01 FW-F-01 FW-F-01 FW-F-01 FW-F-01
                 FW-F-01 FW-F-01 FW-F-01 FW-F-01 FW-F-01
              DEPENDING ON FW-ELE.
           GO TO FEEENT-BEGIN-ENTRY.

      **********************************************************
      *    SET FEE CODE ELEMENTS
      **********************************************************
       FW-F-01.
           MOVE VDUBUF TO CDB-CODE.
           PERFORM GET-CODE.
           IF IO-FG = 9
              GO TO FEEENT-ENTER-ELE.

           MOVE VDUBUF TO FW-FEECODE(FW-ELE).
           MOVE "S  A150  1." TO VDU.
           MOVE FW-ELE TO VDUROW.
           MOVE CDB-DESC TO VDUBUF FW-FEEDESC(FW-ELE).
           PERFORM SCREENIO.

           MOVE CDB-GC-GLFEEFINCHG TO FW-FEEFINCHG(FW-ELE).

      *-----------------------------------------------------------
       ENTER-FEE-AMT1.
           MOVE "^-BACK, ENTER FEE AMOUNT:" TO MESS.
           PERFORM SEND-WSEL-LEGEND.

           MOVE "ENNN042  2." TO VDU.
           MOVE FW-ELE TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              PERFORM FEEENT-RESET-LINE
              GO TO FEEENT-BEGIN-ENTRY.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FEE-AMT1.

           MOVE VDUNUM2 TO FW-FEEAMT(FW-ELE).

           IF VDUNUM2 > CDB-GC-FEE-MAXAMT
              MOVE "INVALID: AMOUNT IS GREATER THAN THE LIMIT" TO MESS
              PERFORM SEND-MESS
              MOVE "RESETTING FEE CODE LINE" TO MESS
              PERFORM SEND-MESS
              PERFORM FEEENT-WIN-RESET-ELE
              GO TO FEEENT-ENTER-ELE.

           MOVE VDUNUM2 TO FW-FEEAMT(FW-ELE).


      * SEND BRANCH DEFAULT OUR FEE CODE:
           MOVE "E  A010   ." TO VDU.
           MOVE FW-ELE TO VDUROW.
           MOVE 3 TO VDUCOL.
           MOVE VDUFIELD TO MESS.
      * THIS WAS CHECKING IF THEY HAVE ALREADY ENTERED VALUES, I CHANGED
      * IT TO CHECK THE ACTUAL FIELD IT IS STORED IN, NOT A FLDVAL OF THE
      * FORM FIELD !
      *    CALL FLDVAL USING STAT MESS VDUBUF.
      *    IF VDUBUF = " "
           IF FW-FEEOURS(FW-ELE) = " "
              IF LC-SALEFIN = "Y"
                 MOVE BR-OUR-FEE TO VDUBUF FW-FEEOURS(FW-ELE)
              ELSE
                 MOVE "Y" TO VDUBUF FW-FEEOURS(FW-ELE)
              END-IF
      *       CALL XFER USING STAT MESS VDUBUF
      *       CALL WRFORM USING STAT MESS.
              MOVE VDUBUF TO WR-BUF
              MOVE MESS TO WR-LIST
              PERFORM CALL-WRFORM.

      *-----------------------------------------------------------
       ENTER-FEEOURS1.
           IF LC-SALEFIN = "Y"
              MOVE "^, IS FEE FINANCE COMPANIES (Y/N/M)?:" TO MESS
              PERFORM SEND-WSEL-LEGEND
           ELSE
              MOVE "^, ENTER 'Y' ONLY:" TO MESS
              PERFORM SEND-WSEL-LEGEND.
           MOVE "E  A010  3." TO VDU.
           MOVE FW-ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FEE-AMT1.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FEEOURS1.
           IF LC-SALEFIN = "Y"
              IF NOT (VDUBUF = "Y" OR "N" OR "M")
                 GO TO ENTER-FEEOURS1
              ELSE
                 NEXT SENTENCE
           ELSE
              IF NOT (VDUBUF = "Y")
                 GO TO ENTER-FEEOURS1.
           MOVE VDUBUF TO FW-FEEOURS(FW-ELE).

           PERFORM FEEENT-WIN-RUNNING-TOTAL.
           GO TO FEEENT-CREATE-ROUTINE.

      *-----------------------------------------------------------
       FEEENT-WIN-EXIT.
           PERFORM FEEENT-WIN-SET-LOANFEES.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FEEENT-SCREEN.
           PERFORM CLOSE-WINDOW.

      ******************************************************************
       FEEENT-WIN-RESET-ELE SECTION.
      ******************************************************************
           MOVE FW-ELE TO SUB.
           PERFORM FEEENT-WIN-SET-WORKFEES-DO.

           MOVE FEESPEC(FW-ELE) TO VDU.
           MOVE "S"             TO VDUACTION.
           MOVE FW-ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE FW-FEECODE(FW-ELE) TO VDUBUF.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF FW-FEECODE(FW-ELE) = SPACES
               MOVE SPACES TO CDB-DESC
           ELSE
           MOVE FW-FEECODE(FW-ELE) TO CDB-CODE
           PERFORM GET-CODE
           IF IO-FG = 9
              MOVE "NOT ON FILE" TO CDB-DESC.

           MOVE "S  A150  1." TO VDU.
           MOVE FW-ELE TO VDUROW.
           MOVE CDB-DESC TO VDUBUF.
           MOVE CDB-DESC TO FW-FEEDESC(FW-ELE).
           PERFORM SCREENIO.

           MOVE CDB-GC-GLFEEFINCHG TO FW-FEEFINCHG(FW-ELE).

           MOVE "ENNN042  2."     TO VDU.
           MOVE "S"               TO VDUACTION.
           MOVE FW-ELE            TO VDUROW.
           MOVE FW-FEEAMT(FW-ELE) TO VDUNUM2.
           PERFORM SCREENIO.

       FEEENT-WIN-RESET-ELE-EXIT.
           EXIT.

      ***********************************************
       FEEENT-RESET-LINE SECTION.
      ***********************************************
           IF FW-FEEOURS(FW-ELE) = " "
              MOVE " " TO FW-FEECODE(FW-ELE)
                          FW-FEEOURS(FW-ELE)
                          FW-FEEFINCHG(FW-ELE)
              MOVE 0 TO FW-FEEAMT(FW-ELE)
           ELSE
              MOVE FW-ELE TO SUB
              PERFORM FEEENT-WIN-SET-WORKFEES-DO.
           MOVE FW-ELE TO SUB.
           PERFORM FEEENT-WIN-LINE-DISPLAY.

      ***********************************************
       FEEENT-WIN-RUNNING-TOTAL SECTION.
      ***********************************************

      * FEEENT GET RUNNING FEE AMOUNTS
           MOVE 0 TO SUB.
           MOVE 0 TO FW-FEE-TOTAL(1)
                     FW-FEE-TOTAL(2)
                     FW-FEE-TOTAL(3).
           MOVE " " TO FW-FEE-TOTAL-BUFFER.

       FEEENT-WIN-RUNNING-TOTAL-NXT.
           ADD 1 TO SUB.
           IF FW-FEEOURS(SUB) = "Y"
              ADD FW-FEEAMT(SUB) TO FW-FEE-TOTAL(1)
           ELSE
           IF FW-FEEOURS(SUB) = "N"
              ADD FW-FEEAMT(SUB) TO FW-FEE-TOTAL(2)
           ELSE
              ADD FW-FEEAMT(SUB) TO FW-FEE-TOTAL(3).
           IF SUB < 10
              GO TO FEEENT-WIN-RUNNING-TOTAL-NXT.

           MOVE 1 TO SUB.
           IF FW-FEE-TOTAL(1) NOT = 0
              MOVE "FINANCE CO." TO FW-FEE-TOTAL-P(SUB)
              MOVE FW-FEE-TOTAL(1) TO FW-FEE-TOTAL-A(SUB)
              ADD 1 TO SUB.
           IF FW-FEE-TOTAL(2) NOT = 0
              MOVE "DEALER FEES" TO FW-FEE-TOTAL-P(SUB)
              MOVE FW-FEE-TOTAL(2) TO FW-FEE-TOTAL-A(SUB)
              ADD 1 TO SUB.
           IF FW-FEE-TOTAL(3) NOT = 0
              MOVE "MEMO FEES" TO FW-FEE-TOTAL-P(SUB)
              MOVE FW-FEE-TOTAL(3) TO FW-FEE-TOTAL-A(SUB).

           MOVE FW-FEE-TOTAL-LIST TO WR-LIST.
           MOVE FW-FEE-TOTAL-BUFFER TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***********************************************
       FEEENT-WIN-PURGE SECTION.
      ***********************************************
           MOVE " " TO FW-FEECODE(FW-ELE)
                       FW-FEEOURS(FW-ELE)
                       FW-FEEFINCHG(FW-ELE).
           MOVE 0 TO FW-FEEAMT(FW-ELE).

      *----MOVE FW-FIRST-FIELD TO SUB.
           MOVE FW-ELE TO SUB.
       FEEENT-WIN-PURGE-AGAIN.
           IF FW-FEECODE(SUB) = " "
              MOVE FW-FEECODE(SUB + 1) TO FW-FEECODE(SUB)
              MOVE FW-FEEAMT(SUB + 1) TO FW-FEEAMT(SUB)
              MOVE FW-FEEOURS(SUB + 1) TO FW-FEEOURS(SUB)
              MOVE FW-FEEFINCHG(SUB + 1) TO FW-FEEFINCHG(SUB)
              MOVE " " TO FW-FEECODE(SUB + 1)
                          FW-FEEOURS(SUB + 1)
                          FW-FEEFINCHG(SUB + 1)
              MOVE 0 TO FW-FEEAMT(SUB + 1).
           IF ((BR-FORCE-VALID-CITY-COUNTY = "Y") AND
               (SP-INSURANCE-WRITTEN = " " OR "Y") AND
                (LC-SMALLOAN = "Y"))
              IF SUB < 3
                 ADD 1 TO SUB
                 GO TO FEEENT-WIN-PURGE-AGAIN
              END-IF
           ELSE
           IF SUB < 9
              ADD 1 TO SUB
              GO TO FEEENT-WIN-PURGE-AGAIN.

           PERFORM FEEENT-WIN-LINE-DISPLAY
               VARYING SUB FROM 1 BY 1
                 UNTIL SUB > 10.
           PERFORM FEEENT-WIN-RUNNING-TOTAL.

       FEEENT-WIN-PURGE-EXIT.
           EXIT.

      ***********************************************
       FEEENT-WIN-LINE-DISPLAY SECTION.
      ***********************************************
           IF FW-FEECODE(SUB) NOT = " "
              MOVE FW-FEECODE(SUB) TO FW-FEECODE-BUF
                                      CDB-CODE
              PERFORM GET-CODE
              MOVE CDB-DESC TO FW-FEEDESC-BUF
              MOVE FW-FEEAMT(SUB) TO FW-FEEAMT-BUF
              MOVE FW-FEEOURS(SUB) TO FW-FEEOURS-BUF
           ELSE
              MOVE " " TO FW-WIN-BUF.

           MOVE SUB TO FW-LIST1
                       FW-LIST2
                       FW-LIST3
                       FW-LIST4.
           MOVE FW-WIN-LIST TO WR-LIST.
           MOVE FW-WIN-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

