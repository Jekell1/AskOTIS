      * COPYMEMBER: LIBWI/RVENT
      **************************************************************************
      *                  (RVENT)
      *    REVERSE CONVERTED TRANSACTION ENTRY.
      * REV:
      *  080395 JTG CORRECTED ENTRY OF PRINBAL > LNAMT RE: OT2BAL
      *              AND ADDED HELP MESSAGES
      *  081595 JTG CORRECTED DISPLAY OF INTPAID ON 'DF' TRCD
      *  081595 JTG ADDED ENTRY OF RVENTX-PRIOR-PAYAMT
      *  092895 JTG STOPPED 'PA' TRCD WHEN 'NSF'
      *  071196 MJD ENLARGED PDTH DATE FIELDS TO 9(6).
      *  033099 GLF ADD TEST OF TRCD'S 'PN' AND 'PZ' FOR TFC
      *  990413 JTG CORRECTED RECALC OF CONTR PDTH ROUTINE WAS NOT TAKING OUT
      *             APPLIED TO INTEREST & LATECHARGES            TFC
      *  990902 JTG CHANGED TO ALLOW FOR ENTRY OF APPLIED OTHER FIELDS RVCHFG(4)
      *             AND ENTRY OF NEGATIVE FIELDS
      *             ALSO  STOPPED INPUT OF APPLIED TO OTHER UNTIL THIS ROUTINE
      *             MODIFIED TO ALLOW ENTRY OF GL OFFSETS RE: ( LXFILE )
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      * 20171031 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 190417 ADDED 'S' TO THIS IF, THESE ARE UCCC LATE CHARGE FORMULAS
      *             EXCEPT "A" AND "C"
      *             IF SP-LCFRMLA = "A" OR "C" OR "F" OR "H"
      *                         OR "I" OR "J" OR "K" OR "L" OR "M"
      *                                         WORLD PR#1369
      * BAH 2023.0328 ADDED SP-LCFRMLA "T" FOR "WI" UCCC FORMULA #1597
      **************************************************************************
       RVENT-WIN SECTION.
           MOVE 15 TO WINDOW-LINES.
           MOVE 78 TO WINDOW-COLUMNS.
           MOVE 9 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE "WI/RVENT" TO WINDOW-FORM-NAM.
           MOVE "RVENT" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY RVENT-SCREEN.

           MOVE RVENT-FORM-FIELDS TO WR-BUF.
           MOVE RVENT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO VDU-WIMENU-SELECTION.
           PERFORM RV-ENTRY-MODULE.

       RVENT-WIN-EXIT.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY RVENT-SCREEN.
           PERFORM CLOSE-WINDOW.


      *************************************************
       RV-ENTRY-MODULE SECTION.
      *************************************************
       RV-BEGIN-ENTRY.

      * SOME LATE CHARGE FORMULA'S DON'T REQUIRE  LCPAID & LCPARTICAL SET
      * THE FOLLOWING ARE THE ONES THAT DO:
           IF SP-LCFRMLA = "A" OR "C" OR "F" OR "H"
                                OR "I" OR "J" OR "K" OR "L" OR "M"
                                 OR "S"
              MOVE " " TO RVCHFG(13)
                          RVCHFG(14)
           ELSE
              MOVE "N" TO RVCHFG(13)
                          RVCHFG(14)
              MOVE "S  A090130." TO VDU
              PERFORM SCREENIO
              MOVE "S  A090140." TO VDU
              PERFORM SCREENIO.
           MOVE 1 TO RV-ONE.
           MOVE 0 TO RV-ELE.
           PERFORM RV-CLEAR-AMOUNTS.
       RV-CREATE-ROUTINE.
           ADD RV-ONE TO RV-ELE.
           IF RV-ELE < 1
              MOVE 1 TO RV-ELE.
           IF RV-ELE NOT > RVMAX
              GO TO RV-ENTER-ELE.
       RV-MODIFY-ROUTINE.
           MOVE "F1-CLEAR, ^-BACK, F7-ACCEPT:" TO RV-LEGEND.
           PERFORM SEND-WSEL-RV-LEGEND.
           MOVE "E  A010WSELX." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM RV-CLEAR-AMOUNTS
              MOVE "X" TO RVENT-ERRCD
              GO TO RV-ENTRY-MODULE-EXIT.
           IF VDUSTATUS = "1>"
              MOVE 99 TO RV-ELE
              PERFORM RV-EDIT-AMOUNTS
              IF RV-ELE NOT = 99
                 GO TO RV-ENTER-ELE
              ELSE
                 MOVE RVENTX-FIELDS TO RVENT-FIELDS
                 GO TO RV-ENTRY-MODULE-EXIT.
           IF VDUSTATUS = "1U"
              MOVE 19 TO RV-ELE
              GO TO RV-ENTER-ELE.
           GO TO RV-MODIFY-ROUTINE.

       RV-ENTER-ELE.
           IF RVCHFG(RV-ELE) = "N"
              GO TO RV-CREATE-ROUTINE.

           MOVE RV-LEGEND-ELE(RV-ELE) TO RV-LEGEND.
           PERFORM SEND-WSEL-RV-LEGEND.

           MOVE RVSPEC(RV-ELE) TO VDU.
           MOVE RV-ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       RV-TEST-F1.
           IF VDUSTATUS = "10"
              PERFORM RV-CLEAR-AMOUNTS
              MOVE "X" TO RVENT-ERRCD
              GO TO RV-ENTRY-MODULE-EXIT.
       RV-TEST-UP.
           IF VDUSTATUS = "1U"
              MOVE -1 TO RV-ONE
              GO TO RV-CREATE-ROUTINE.
       RV-TEST-DOWN.
           IF VDUSTATUS = "1D"
              MOVE 1 TO RV-ONE
              GO TO RV-GOTO-FIELD.

           IF VDUSTATUS NOT = "00"
              GO TO RV-ENTER-ELE.

           MOVE 1 TO RV-ONE.
       RV-GOTO-FIELD.
           GO TO RV-F-01 RV-F-02 RV-F-03 RV-F-04
                 RV-F-05 RV-F-06 RV-F-07 RV-F-08
                 RV-F-09 RV-F-10 RV-F-11 RV-F-12
                 RV-F-13 RV-F-14 RV-F-15 RV-F-16
                 RV-F-17 RV-F-18 RV-F-19
                 DEPENDING ON RV-ELE.
           GO TO RV-BEGIN-ENTRY.

      *******************************************
      *    SET RESERVE ENTRY RECORD ELEMENTS
      *******************************************
       RV-F-01.
      * SPEC-REC HAS THESE AS "ENNN060", NOT USING DATE ENTRY!
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO RVENTX-PAYDATE.
           IF VDUNUM0 = 0
              MOVE "ENTER THE PAYMENT DATE OF REVERSED TRANSACTION"
                                                          TO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.

           IF RVENTX-PAYDATE NOT = LN-DATE-PAID-LAST
              MOVE SPACES TO MESS
              MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO ALP-DATE-EDIT
              STRING
               "PAYMENT DATE MUST MATCH TRUE LAST PAYMENT DATE OF: "
                         ALP-DATE-EDIT
                           DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.
           GO TO RV-CREATE-ROUTINE.
       RV-F-02.
           MOVE VDUBUF TO RVENTX-TRCD.
           IF VDUBUF = " "
              MOVE "MUST ENTER TRAN CODE OF REVERSED TRANSACTION"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.

      * STOPPED INPUT OF APPLIED TO OTHER UNTIL THIS ROUTINE IS
      * MODIFIED TO ALLOW ENTRY OF GL OFFSETS RE: ( LXFILE )
           MOVE "N" TO RVCHFG(4).

           MOVE " " TO RVCHFG(5) RVCHFG(6) RVCHFG(7).
           MOVE "S  A090040." TO VDU.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE:
      *          IF THIS LOGIC TO STOP "PA" AND "NSF" IS CHANGED FOR
      *          LOCK BOXES, LONPF2.C MUST BE CHANGED
      *          EX.
      *              OR (CONV-REV-FLAG = "Y" AND HOLD-LP-REFNO = "NSF"
      *          CAN'T UPDATE TY-CKWRITTEN(5) WHEN 'LBOX' NSF
      *          THIS IS NOT A BANK TRANSACTION
      *
      *          JTG 000926  (MFC PR#2227)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF RVENTX-TRCD = "PA"
                  AND RVENT-REFNO = "NSF"
              MOVE "NSF'S ARE NOT ALLOWED ON ALLOTMENT PAYMENTS"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.
           IF RVENTX-TRCD = "PY" OR "PA" OR "PE" OR "AH"
                                 OR "PN" OR "PZ" OR "IN"
              MOVE "N" TO RVCHFG(5)
              MOVE 0 TO RVENTX-APOT2
              MOVE "S  A090050." TO VDU
              PERFORM SCREENIO
              IF IBPC-FG = "P"
                 MOVE "N" TO RVCHFG(6)
                 MOVE 0 TO RVENTX-APINT
                           RVENTX-INTPAID
                 MOVE "S  A090060." TO VDU
                 PERFORM SCREENIO
              ELSE
                 NEXT SENTENCE
           ELSE
           IF RVENTX-TRCD = "Z2"
              MOVE "N" TO RVCHFG(4) RVCHFG(6) RVCHFG(7)
              MOVE 0 TO RVENTX-APOTH
              MOVE "S  A090040." TO VDU
              PERFORM SCREENIO
              MOVE 0 TO RVENTX-APINT
                        RVENTX-INTPAID
              MOVE "S  A090060." TO VDU
              PERFORM SCREENIO
              MOVE 0 TO RVENTX-APLC
              MOVE "S  A090070." TO VDU
              PERFORM SCREENIO
           ELSE
           IF RVENTX-DF-POSTING
              IF RVENTX-TRCD = "DF"
                 MOVE 1 TO  NDTE-HOLD
              ELSE
                 MOVE RVENTX-NO-DEF TO NDTE-HOLD
              END-IF
              IF NDTE-HOLD > LN-TOTNODEF
                 MOVE LN-TOTNODEF TO RVENT-ALP-PDTH-DATE
                 MOVE " " TO MESS
                 STRING
                   "EXCEEDS NO OF DEFERMENTS TAKEN TO DATE OF: "
                      RVENT-ALP-PDTH-DATE
                        DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
                 GO TO RV-ENTER-ELE
              END-IF
              MOVE "N" TO RVCHFG(4) RVCHFG(5) RVCHFG(7)
              MOVE 0 TO RVENTX-APOTH
              MOVE "S  A090040." TO VDU
              PERFORM SCREENIO
              MOVE 0 TO RVENTX-APOT2
              MOVE "S  A090050." TO VDU
              PERFORM SCREENIO
              MOVE 0 TO RVENTX-APLC
              MOVE "S  A090070." TO VDU
              PERFORM SCREENIO
           ELSE
              MOVE "PY, PA, PE, Z2, OR DEFERMENTS ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.
           GO TO RV-CREATE-ROUTINE.
       RV-F-03.
           MOVE VDUNUM2 TO RVENTX-TRAMT.
      *    IF RVENTX-TRAMT NOT = LN-AMT-PAID-LAST
      *       MOVE " " TO MESS
      *       MOVE LN-AMT-PAID-LAST TO RVENT-ALP-AMT
      *       STRING
      *     "PAYMENT MUST MATCH TRUE LAST PAYMENT AMOUNT OF: $"
      *                  RVENT-ALP-AMT
      *                    DELIMITED BY SIZE INTO MESS
      *       PERFORM SEND-MESS
      *       STRING
      *     "* NOTE -> IF THIS IS WRONG **STOP** CALL HOME OFFICE!!!"
      *                    DELIMITED BY SIZE INTO MESS
      *       PERFORM SEND-MESS
      *       GO TO RV-ENTER-ELE.
           IF RVENTX-TRCD = "Z2"
              MOVE "SNNN052050." TO VDU
              MOVE RVENTX-TRAMT TO RVENTX-APOT2
                                   VDUNUM2
              PERFORM SCREENIO.
           IF RVENTX-DF-POSTING
              MOVE "SNNN052060." TO VDU
              MOVE RVENTX-TRAMT TO VDUNUM2
                                   RVENTX-APINT
                                   RVENTX-INTPAID
              PERFORM SCREENIO.
           GO TO RV-CREATE-ROUTINE.
       RV-F-04.
           MOVE VDUNUM2 TO RVENTX-APOTH.
           GO TO RV-CREATE-ROUTINE.
       RV-F-05.
           MOVE VDUNUM2 TO RVENTX-APOT2.
           GO TO RV-CREATE-ROUTINE.
       RV-F-06.
           MOVE VDUNUM2 TO RVENTX-APINT
                           RVENTX-INTPAID.
           GO TO RV-CREATE-ROUTINE.
       RV-F-07.
           MOVE VDUNUM2 TO RVENTX-APLC.
           IF RVENTX-TRAMT < (RVENTX-APOTH + RVENTX-APOT2
                                + RVENTX-APINT + RVENTX-APLC)
              MOVE " " TO MESS
              STRING
                 "THE TOTAL AMOUNT APPLIED CAN'T EXCEED "
                   "THE TRANSACTION AMOUNT!"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.

           GO TO RV-CREATE-ROUTINE.
       RV-F-08.
      * SPEC-REC HAS THESE AS "ENNN060", NOT USING DATE ENTRY!
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO RVENTX-PRIOR-PAYDATE.
           IF VDUNUM0 = 0
              MOVE "MUST ENTER PRIOR PAYMENT DATE!" TO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.

           IF RVENTX-PRIOR-PAYDATE > RVENTX-PAYDATE
              MOVE SPACES TO MESS
              STRING "PRIOR PAYMENT CAN'T BE AFTER THE "
                       "PAYMENT YOUR REVERSING"
                          DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.

           IF LN-LOANDATE > RVENTX-PRIOR-PAYDATE
              MOVE SPACES TO MESS
              STRING "PRIOR PAYMENT DATE CAN'T BE PRIOR TO "
                       "THE LOANS CONTRACT DATE?"
                          DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.

           GO TO RV-CREATE-ROUTINE.
       RV-F-09.
           GO TO RV-CREATE-ROUTINE.
       RV-F-10.
           MOVE VDUNUM2 TO RVENTX-PRIOR-PAYAMT.
           IF RVENTX-PRIOR-PAYAMT = 0
              IF LN-INTDATE NOT = RVENTX-PRIOR-PAYDATE
                 MOVE "WASN'T THERE A PRIOR PAYMENT AMOUNT!" TO MESS
                 PERFORM SEND-MESS.

      *-------------------------------------------------
      *    CALCULATE RESULTING PAID THRU
      *-------------------------------------------------
           MOVE LN-TOTPAYMNTD TO RVENT-HOLDER.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           IF (RVENTX-TRCD = "PY" OR "PO" OR "PA"  OR "AH" OR "PR"
                                   OR "PC" OR "PP" OR "PN" OR "PZ")
                    OR (RVENTX-DF-POSTING)
              COMPUTE LN-TOTPAYMNTD =
                 LN-TOTPAYMNTD
                      - (RVENTX-TRAMT - RVENTX-APOTH - RVENTX-APOT2
                                      - RVENTX-APINT - RVENTX-APLC)
              IF SP-CONTRFRMLA = "A"
                 SUBTRACT RVENTX-APLC FROM LN-TOTPAYMNTD.
           IF RVENTX-TRCD = "PY" OR "PO" OR "PA" OR "AH" OR "PR"
                                 OR "PC" OR "PP" OR "PN" OR "PZ"
              SUBTRACT RVENTX-APINT FROM LN-TOTPAYMNTD.

      * JTG CORRECTED WAS NOT TAKING OUT APPLIED TO INT * LC    TFC 041399
      *       IF SP-CONTRFRMLA = "A"
      *          ADD RVENTX-APLC TO LN-TOTPAYMNTD.
      *    IF RVENTX-TRCD = "PY" OR "PO" OR "PA" OR "AH"
      *                   OR "PR" OR "PC" OR "PP" OR "PN" OR "PZ"
      *       ADD RVENTX-APINT TO LN-TOTPAYMNTD.
           PERFORM PAID-THRU-CALCULATION.

           MOVE RVENT-HOLDER TO LN-TOTPAYMNTD.
      *-------------------------------------------------
      * SEND RESULTING PAID THRU:
           MOVE PDTH-DATE-FULL TO RVENTX-PDTH-DATE
                                  RVENT-ALP-PDTH-DATE.
           MOVE "S  A060110." TO VDU.
           MOVE RVENT-ALP-PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDUBUF.
           PERFORM SCREENIO.

           GO TO RV-CREATE-ROUTINE.
       RV-F-11.
      * SPEC-REC HAS THESE AS "ENNN060", NOT USING DATE ENTRY!
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO RVENTX-PDTH-DATE.
           GO TO RV-CREATE-ROUTINE.
       RV-F-12.
      * SPEC-REC HAS THESE AS "ENNN060", NOT USING DATE ENTRY!
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO RVENTX-LCPDTH-DATE.
           IF DATE-YYYYMMDD-MM < 1 OR > 12
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.

           IF LN-LCPDTH-DATE < RVENTX-LCPDTH-DATE
              MOVE SPACES TO MESS
              MOVE LN-LCPDTH-DATE TO RVENT-ALP-PDTH-DATE
              STRING "L/C PAID THRU CAN'T BE GREATER "
                      "THAN CURRENT LCPDTH OF: "
                        RVENT-ALP-PDTH-DATE
                         DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.

           MOVE LN-OTHBAL TO RVENTX-OTHBAL
                             RV-OTHBAL.
           COMPUTE RVENTX-OT2BAL
                       RV-OT2BAL = LN-OT2BAL + RVENTX-APOT2.
           IF RVENTX-DF-POSTING
              COMPUTE RVENTX-INTBAL
                          RV-INTBAL = LN-INTBAL
           ELSE
              COMPUTE RVENTX-INTBAL
                          RV-INTBAL = LN-INTBAL.
           COMPUTE RVENTX-LCBAL
                       RV-LCBAL = LN-LCBAL.
           MOVE RV-DISPLAY-BUF TO WR-BUF.
           MOVE RV-DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO RV-CREATE-ROUTINE.
       RV-F-13.
      * SPEC-REC HAS THESE AS "ENNN060", NOT USING DATE ENTRY!
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO RVENTX-LC-DATE.
           IF VDUNUM0 NOT = 0
              MOVE TRANS-DATE     TO NUM-DATE
              MOVE RVENTX-LC-DATE TO SYS-DATE
              MOVE LN-1STPYDA     TO S-DD
              IF SYS-DATE > NUM-DATE
                 MOVE "LC GRACE PERIOD CAN NOT BE IN THE FUTURE"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO RV-ENTER-ELE
              ELSE
                 MOVE LN-1STPYDATE   TO NUM-DATE
                 MOVE RVENTX-LC-DATE TO SYS-DATE
                 MOVE LN-1STPYDA     TO S-DD
                 IF SYS-DATE < NUM-DATE
                    MOVE
              "LC GRACE PERIOD CAN'T BE BEFORE 1ST PAY DATE" TO MESS
                    PERFORM SEND-MESS
                    GO TO RV-ENTER-ELE.
           GO TO RV-CREATE-ROUTINE.
       RV-F-14.
           MOVE VDUNUM2 TO RVENTX-LC-AMT.
           GO TO RV-CREATE-ROUTINE.
       RV-F-15.
           MOVE VDUNUM2 TO RVENTX-OTHBAL.
           GO TO RV-CREATE-ROUTINE.
       RV-F-16.
           MOVE VDUNUM2 TO RVENTX-OT2BAL.
           GO TO RV-CREATE-ROUTINE.
       RV-F-17.
           MOVE VDUNUM2 TO RVENTX-INTBAL.
           GO TO RV-CREATE-ROUTINE.
       RV-F-18.
           MOVE VDUNUM2 TO RVENTX-LCBAL.
           GO TO RV-CREATE-ROUTINE.
       RV-F-19.
           MOVE VDUNUM2 TO RVENTX-PRINBAL.
           IF RVENTX-PRINBAL NOT =
                  LN-CURBAL + (RVENTX-TRAMT - RVENTX-APOTH
                                   - RVENTX-APINT - RVENTX-APLC)
              MOVE "PRINBAL SHOULD BE " TO MESS
              COMPUTE MESS-X-MON-3 =
                  LN-CURBAL + (RVENTX-TRAMT - RVENTX-APOTH
                                   - RVENTX-APINT - RVENTX-APLC)
              PERFORM SEND-MESS
              GO TO RV-ENTER-ELE.
           GO TO RV-CREATE-ROUTINE.

       RV-ENTRY-MODULE-EXIT.
           EXIT.

      ******************************************
       RV-CLEAR-AMOUNTS SECTION.
      ******************************************
           MOVE 0 TO RVENTX-PAYDATE RVENTX-TRAMT RVENTX-APOTH
                     RVENTX-APOT2 RVENTX-APINT RVENTX-APLC
                     RVENTX-PRIOR-PAYDATE
                     RVENTX-PRIOR-PAYAMT
                     RVENTX-PDTH-DATE RVENTX-LCPDTH-DATE
                     RVENTX-OTHBAL RVENTX-OT2BAL RVENTX-INTBAL
                     RVENTX-LCBAL RVENTX-PRINBAL RVENTX-LC-DATE
                     RVENTX-LC-AMT RVENT-INTPAID.
           MOVE SPACE TO RVENTX-TRCD.

      ******************************************
       RV-EDIT-AMOUNTS SECTION.
      ******************************************
           IF RVENTX-OTHBAL NOT = LP-OTHBAL
              MOVE "   CAN NOT CHANGE OTHER BAL" TO MESS
              MOVE RVENTX-TRCD TO MESS
              PERFORM SEND-MESS
              MOVE 11 TO RV-ELE
           ELSE
           IF LN-LNAMT < (RVENTX-PRINBAL - LN-OT2BAL)
              MOVE "PRIN BALANCE CAN NOT EXCEED" TO MESS
              MOVE LN-LNAMT TO MESS-X-MON-3
              PERFORM SEND-MESS
              MOVE 3 TO RV-ELE
           ELSE
           IF RVENTX-TRCD = "Z2"
                  AND (RVENTX-TRAMT NOT = RVENTX-APOT2)
              MOVE "APPLIED TO APOT2 MUST = TRAN AMT ON Z2 TRANCODE"
                                                             TO MESS
              PERFORM SEND-MESS
              MOVE 3 TO RV-ELE
           ELSE
           IF RVENTX-OT2BAL NOT = (RVENTX-APOT2 + LN-OT2BAL)
              MOVE "OTHER 2 BALANCE MUST = " TO MESS
              COMPUTE MESS-X-MON-3 = (RVENTX-APOT2 + LN-OT2BAL)
              PERFORM SEND-MESS
              MOVE 5 TO RV-ELE
           ELSE
           IF RVENTX-PRINBAL NOT =
                  LN-CURBAL + (RVENTX-TRAMT - RVENTX-APOTH
                                   - RVENTX-APINT - RVENTX-APLC)
              MOVE "PRINBAL SHOULD BE " TO MESS
              COMPUTE MESS-X-MON-3 =
                  LN-CURBAL + (RVENTX-TRAMT - RVENTX-APOTH
                                       - RVENTX-APINT - RVENTX-APLC)
              PERFORM SEND-MESS
              MOVE 19 TO RV-ELE.

           COMPUTE LP-APCUR =
               (RVENTX-TRAMT - RVENTX-APOTH
                         - RVENTX-APINT - RVENTX-APLC).

           COMPUTE LP-CURBAL = LP-CURBAL - LP-APCUR.

       RV-EDIT-AMOUNTS-EXIT.
           EXIT.

      ******************************************
       SEND-WSEL-RV-LEGEND SECTION.
      ******************************************
           MOVE RV-LEGEND TO WR-BUF.
           MOVE WSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
