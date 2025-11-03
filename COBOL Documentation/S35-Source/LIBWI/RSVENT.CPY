      * COPYMEMBER: LIBWI/RSVENT
      **************************************************************************
      *                  (RSVENT)
      *    RESERVE AMOUNTS ENTRY WINDOW
      * REV:
      *  MJD 081695 ADDED GP-DLRVPRIN-3-FG AND GP-DLRVCGHS-3-FG LOGIC TO BLOCK
      *             ENTRY OF LEVEL 3 OF LN-DLRVPRIN AND LN-DLRVCHGS
      *  BAH 012596 ADDED RSVENT-LOANTYPE TO TEST 'IB' LOANS NO PARTICIPATION
      *             ALLOWED, #E094
      *  JTG 110896 FIX DISPLAY OF ANTICERN RE:DLDISC
      *  JTG 070198 CHANGED TO FORCE RE-INPUT OF DISCOUNT IF IT CAUSES NEGATIVE
      *             DEALER CHECK
      *  JTG 102198 ADDED TEST ON DL-PARVPAID-FRMLA-IB INSTANT AUTO #744
      *  JTG 990310 ALLOWED PARTICIPATION HELD ON IB ACCOUNTS, AUTOMOTIVE CREDIT
      *  JTG 990318 ADDED LOGIC FOR SP-DLDISC-RVPRIN1-PCNT, TFC #008
      *                             SP-LNCOST-FRMLA 'D'
      *  MJD 990726 ADDED PASSWORD TO DLR RESV PART HELD/PAID ON IB ACCOUNTS
      *  MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  JTG 991214 ADDED SP-DLDISC-RVPRIN1-AF-PCNT, TFC #058
      *  JTG 000104 CHANGED SP-DLDISC-RVPRIN1-AF-PCNT, TFC #058
      *             TO ACCEPT THE AMOUNT INPUTED IN RVPRIN1 WHEN IT IS LESS
      *             THAN TO COMPUTED AMOUNT
      *  BAH 000313 TEST FOR SP-DLDISC-RVPRIN1-AF-PCNT NOT = 202.02 AND
      *                      SP-DLDISC-RVPRIN1-PCNT NOT = 202.02, KEVIN
      *  BLV 001018 TEST IF SP-DLDISC-RVPRIN1-AF-PCNT = 202.02 OR
      *             SP-DLDISC-RVPRIN1-PCNT = 202.02, FORCE 0 TO THEM &
      *             REMOVE SPECIFIC CHECKS WITHIN IF STATEMENTS; THERE
      *             WERE CASES THAT 202.02 WOULD FORCE 0 IN ENTRY
      *  JTG 020905 ADDED LN-DLCOMM-O3-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
      *   CS 060604 ADDED TEST GP-I-AM-SECURITY, IF DEALER BRANCH IS 02 DONT
      *             ALLOW ENTRY IN RSVPRIN(1), IF DL-BR = 01 NOT ACCESS TO
      *             RSVPRIN(2)      SECURITY AUTO PR# 2503
      *   CS 081218 ADD TEST FOR GP-INCL-OTH-IN-EARNED = "R". THIS IS A REGACC
      *             OPTION TO EXCLUDE BOTH ANTICERN(6) AND ANTICERN(4) FROM
      *             ANTICIPATED EARNINGS PR#210
      *   CS 060604 ADDED TEST GP-I-AM-SECURITY, IF DEALER BRANCH IS 04 DONT
      *             ALLOW ENTRY IN RSVPRIN(1) OR RSVPRIN(2)SECURITY AUTO PR#3402
      *  BAH 121002 ADDED FORCE-ENTERED-PRIN1, FORCE-ENTERED-PRIN2 AND
      *                   FORCE-ENTERED-DISC FOR SECURITY AUTO PR# 4116
      *             THEY MUST AT LEAST RETURN THRU THE FIELD
      *   CS 140626 DESTROY WINDOW BEFORE CLOSE
      **************************************************************************
       RSVENT-WIN SECTION.
           MOVE 21 TO WINDOW-LINES.
           MOVE 37 TO WINDOW-COLUMNS.
           MOVE 4 TO WINDOW-BEGIN-Y.
           MOVE 0 TO WINDOW-BEGIN-X.
           MOVE "RSVENT" TO VDU-FORM-NAME.
           MOVE "WI/RSVENT" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY RSVENT-SCREEN.

           MOVE RSVENT-FORM-FIELDS TO WR-BUF.
           MOVE RSVENT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * SET PROMPT IN WINDOW FOR COMM OR DLDISC:
           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMM DEALER INS :" TO MESS
           ELSE
              MOVE "DEALER DISCOUNT :" TO MESS.

           MOVE MESS TO WR-BUF.
           MOVE "07:S." TO WR-LIST.
           PERFORM CALL-WRFORM.

      * STOP INPUT OF DEALER DISC IF SP-DLDISC-RVPRIN1-PCNT    NOT = 0, TFC #008
      * STOP INPUT OF DEALER DISC IF SP-DLDISC-RVPRIN1-AF-PCNT NOT = 0, TFC #058
           IF SP-DLDISC-RVPRIN1-PCNT = 202.02
              MOVE 0 TO SP-DLDISC-RVPRIN1-PCNT.
           IF SP-DLDISC-RVPRIN1-AF-PCNT = 202.02
              MOVE 0 TO SP-DLDISC-RVPRIN1-AF-PCNT.
           IF (SP-DLDISC-RVPRIN1-PCNT    = 0) AND
              (SP-DLDISC-RVPRIN1-AF-PCNT = 0)
              MOVE " " TO RSVCHFG(7)
           ELSE
              MOVE "N" TO RSVCHFG(7).

      * SET PROMPT IN WINDOW FOR LOAN COSTS:
           IF SP-LNCOST-FRMLA = "D"
              MOVE " "                 TO RSVCHFG(10)
              MOVE "LOAN COST  DLCK :" TO MESS
              MOVE "I"                 TO RSVENTX-LNCOST-FG
           ELSE
              MOVE "N"                 TO RSVCHFG(10)
              MOVE " "                 TO MESS
                                          RSVENTX-LNCOST-FG.
           MOVE MESS TO WR-BUF.
           MOVE "10:S 100 101." TO WR-LIST.
           PERFORM CALL-WRFORM.

      * SET PROMPT IN WINDOW FOR DLCOMM-PP-CMSI:
           IF GP-DLCOMM-CMSI-FG = "Y"
              MOVE " "                 TO RSVCHFG(11)
              MOVE "DLCOMM (PP)     :" TO MESS
              MOVE "I"                 TO RSVENTX-DLCOMM-PP-CMSI-FG
           ELSE
              MOVE "N"                 TO RSVCHFG(11)
              MOVE " "                 TO MESS
                                          RSVENTX-DLCOMM-PP-CMSI-FG.
           MOVE MESS TO WR-BUF.
           MOVE "11:S 110 111." TO WR-LIST.
           PERFORM CALL-WRFORM.

      * SET PROMPT IN WINDOW FOR DLCOMM-O3-CMSI:
           IF GP-DLCOMM-CMSI-FG = "Y"
              MOVE " "                 TO RSVCHFG(12)
              MOVE "DLCOMM (O3)     :" TO MESS
              MOVE "I"                 TO RSVENTX-DLCOMM-O3-CMSI-FG
           ELSE
              MOVE "N"                 TO RSVCHFG(12)
              MOVE " "                 TO MESS
                                          RSVENTX-DLCOMM-O3-CMSI-FG.
           MOVE MESS TO WR-BUF.
           MOVE "12:S 120 121." TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE RSVENT-FIELDS TO RSVENTX-FIELDS.

           PERFORM RSV-DISPLAY-FIELDS.
           IF SP-ERNFRMLA(5) = 0
              MOVE "N" TO RSVCHFG(7).

           IF GP-DLRVPRIN-1-FG NOT = "Y"
              MOVE "N" TO RSVCHFG(1).

           IF GP-DLRVPRIN-2-FG NOT = "Y"
              MOVE "N" TO RSVCHFG(2).

           IF GP-DLRVPRIN-3-FG NOT = "Y"
              MOVE "N" TO RSVCHFG(8).

           IF GP-DLRVCHGS-3-FG NOT = "Y"
              MOVE "N" TO RSVCHFG(9).


           MOVE "N"       TO RSVCHFG(7).


      * IF SALES FINANCE "IB" LOAN, DONT ALLOW ANY RSV FROM CHGS
           IF RSVENTX-LOANTYPE = "I"
              MOVE "N" TO RSVCHFG(3) RSVCHFG(4) RSVCHFG(9)
      * ALLOW PARTICIPATION PAID PER DL-PARVPAID-FRMLA-IB
              IF DL-PARVPAID-FRMLA-IB = " "
                 MOVE "N" TO RSVCHFG(6).

           PERFORM RSV-ENTRY-MODULE.

       RSVENT-WIN-EXIT.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY RSVENT-SCREEN.
           PERFORM CLOSE-WINDOW.

      *************************************************
       RSV-ENTRY-MODULE SECTION.
      *************************************************
       RSV-BEGIN-ENTRY.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO RSV-ONE.
           MOVE 0 TO RSV-ELE.
       RSV-CREATE-ROUTINE.
           ADD RSV-ONE TO RSV-ELE.
           IF RSV-ELE < 1
              MOVE 1 TO RSV-ELE.
           IF RSV-ELE > RSVMAX
              IF RSVCHFG(RSVMAX) = "N"
                 MOVE 1 TO RSV-ELE
              ELSE
                 MOVE RSVMAX TO RSV-ELE.
       RSV-ENTER-ELE.
           MOVE "F7, F1-RESET, ENTER FIELD INFO:" TO MESS.
           PERFORM SEND-WSEL-LEGEND.

           IF RSVCHFG(RSV-ELE) = "N"
              IF RSV-ELE = 1
                 MOVE 1 TO RSV-ONE
                 GO TO RSV-CREATE-ROUTINE.
           IF RSVCHFG(RSV-ELE) = "N"
              GO TO RSV-CREATE-ROUTINE.


           MOVE RSVSPEC(RSV-ELE) TO VDU.
           MOVE RSV-ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       RSV-TEST-F1.
           IF VDUSTATUS = "10"
              MOVE RSVENT-FIELDS TO RSVENTX-FIELDS
              PERFORM RSV-DISPLAY-FIELDS
              GO TO RSV-BEGIN-ENTRY.
       RSV-TEST-UP.
           IF VDUSTATUS = "1U"
              MOVE -1 TO RSV-ONE
              GO TO RSV-GOTO-FIELD.
       RSV-TEST-DOWN.
           IF VDUSTATUS = "1D"
              MOVE 1 TO RSV-ONE
              GO TO RSV-GOTO-FIELD.
       RSV-TEST-F7.
           IF VDUSTATUS = "1>"
              PERFORM RSV-SET-DLCHECK
              IF RSV-ERR-FG = 9
                 GO TO RSV-CREATE-ROUTINE
              END-IF
              PERFORM RSV-SET-ANTIC
              IF RSV-ERR-FG = 9
                 GO TO RSV-CREATE-ROUTINE
              END-IF
              MOVE RSVENTX-FIELDS TO RSVENT-FIELDS
              GO TO END-RSV-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO RSV-ENTER-ELE.

           MOVE 1 TO RSV-ONE.
       RSV-GOTO-FIELD.
           GO TO RSV-F-PRIN   RSV-F-PRIN
                 RSV-F-CHGS   RSV-F-CHGS
                 RSV-F-HELD   RSV-F-PAID RSV-F-DLDISC
                 RSV-F-PRIN   RSV-F-CHGS
                 RSV-F-LNCOST RSV-F-DLCOMM-PP
                 RSV-F-DLCOMM-O3
                 DEPENDING ON RSV-ELE.
           GO TO RSV-BEGIN-ENTRY.

      *******************************************
      *    SET RESERVE ENTRY RECORD ELEMENTS
      *******************************************
      *----------------------------------------------
       RSV-F-PRIN.
           IF RSV-ELE = 1
              MOVE "Y" TO FORCE-ENTERED-PRIN1
              MOVE RSV-ELE TO RVNO
           ELSE
           IF RSV-ELE = 2
              MOVE "Y" TO FORCE-ENTERED-PRIN2
              MOVE RSV-ELE TO RVNO
           ELSE
              MOVE 3 TO RVNO.
           MOVE RSVENTX-DLRVPRIN(RVNO) TO RSV-HOLDER.
           IF (RVNO = 1) AND
              (
                (SP-DLDISC-RVPRIN1-PCNT    NOT = 0) OR
                (SP-DLDISC-RVPRIN1-AF-PCNT NOT = 0)
              )
              IF (SP-DLDISC-RVPRIN1-PCNT NOT = 0)
                 COMPUTE RSVENTX-DLDISC ROUNDED =
                      VDUNUM2 * SP-DLDISC-RVPRIN1-PCNT * 0.01
                 COMPUTE RSVENTX-DLRVPRIN(1) =
                      VDUNUM2 - RSVENTX-DLDISC
              ELSE
                 IF (SP-DLDISC-RVPRIN1-AF-PCNT NOT = 0)
                    COMPUTE RSVENTX-DLRVPRIN(1) ROUNDED =
                         RSVENT-AMTFIN
                           * SP-DLDISC-RVPRIN1-AF-PCNT * 0.01
                    IF VDUNUM2 < RSVENTX-DLRVPRIN(1)
                       MOVE 0       TO RSVENTX-DLDISC
                       MOVE VDUNUM2 TO RSVENTX-DLRVPRIN(1)
                       MOVE
               " AMOUNT ENTERED IS LESS THAN COMPUTED RSV PRIN RE: SPR%"
                                                            TO MESS
                       PERFORM SEND-MESS
                    ELSE
                       COMPUTE RSVENTX-DLDISC =
                           VDUNUM2 - RSVENTX-DLRVPRIN(1)
                    END-IF
                 END-IF
              END-IF
              MOVE "SNNN062010." TO VDU
              MOVE RSVENTX-DLRVPRIN(1) TO VDUNUM2
              PERFORM SCREENIO
              MOVE "SNNN062070." TO VDU
              MOVE RSVENTX-DLDISC TO VDUNUM2
              PERFORM SCREENIO
              MOVE "S  A010071." TO VDU
              MOVE "I" TO VDUBUF RSVENTX-DLDISC-FG
              PERFORM SCREENIO
              PERFORM RSV-SET-DLCHECK
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE
              END-IF
              PERFORM RSV-SET-ANTIC
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE
              END-IF
           ELSE
              MOVE VDUNUM2 TO RSVENTX-DLRVPRIN(RVNO)
              PERFORM RSV-SET-DLCHECK
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE.
           IF RSVENTX-DLRVPRIN(RVNO) = RSV-HOLDER
              GO TO RSV-CREATE-ROUTINE.
       CONT-RSV-F-PRIN.
           MOVE "S  A010  1." TO VDU.
           MOVE RSV-ELE TO VDUROW.
           MOVE "I" TO VDUBUF RSVENTX-DLRVPRIN-FG(RVNO).
           PERFORM SCREENIO.

           MOVE "E  A010  1." TO VDU.
           MOVE RSV-ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO RSV-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-RSV-F-PRIN.
           MOVE VDUBUF TO RSVENTX-DLRVPRIN-FG(RVNO).
           IF NOT (RSVENTX-DLRVPRIN-FG(RVNO) = "I" OR "C")
              PERFORM RSV-I-OR-C
              GO TO CONT-RSV-F-PRIN.
           IF RSVENTX-DLRVPRIN-FG(RVNO) = "C"
              MOVE RSVENTX-DLRVPRIN(RVNO)
                                TO PURFLD-DLRVPRIN(RVNO)
              MOVE RSVENTX-DLRVPRIN-FG(RVNO)
                                TO PURFLD-DLRVPRIN-FG(RVNO)
              PERFORM COMPUTE-DLRVPRIN
              MOVE "SNNN062  0." TO VDU
              MOVE RSV-ELE TO VDUROW
              MOVE PURFLD-DLRVPRIN(RVNO) TO VDUNUM2
                                         RSVENTX-DLRVPRIN(RVNO)
              PERFORM SCREENIO
              PERFORM RSV-SET-DLCHECK
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE.

           GO TO RSV-CREATE-ROUTINE.

      *----------------------------------------------
       RSV-F-CHGS.
           IF RSV-ELE = 3 OR 4
              SUBTRACT 2 FROM RSV-ELE GIVING RVNO
           ELSE
              MOVE 3 TO RVNO.
           MOVE RSVENTX-DLRVCHGS(RVNO) TO RSV-HOLDER.
           MOVE VDUNUM2 TO RSVENTX-DLRVCHGS(RVNO).
           PERFORM RSV-SET-ANTIC.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           IF RSVENTX-DLRVCHGS(RVNO) = RSV-HOLDER
              GO TO RSV-CREATE-ROUTINE.
       CONT-RSV-F-CHGS.
           MOVE "S  A010  1." TO VDU.
           MOVE RSV-ELE TO VDUROW.
           MOVE "I" TO VDUBUF RSVENTX-DLRVCHGS-FG(RVNO).
           PERFORM SCREENIO.

           MOVE "E  A010  1." TO VDU.
           MOVE RSV-ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO RSV-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-RSV-F-CHGS.
           MOVE VDUBUF TO RSVENTX-DLRVCHGS-FG(RVNO).
           IF NOT (RSVENTX-DLRVCHGS-FG(RVNO) = "I" OR "C")
              PERFORM RSV-I-OR-C
              GO TO CONT-RSV-F-CHGS.
           IF RSVENTX-DLRVCHGS-FG(RVNO) = "C"
              MOVE RSVENTX-DLRVCHGS(RVNO)
                                TO PURFLD-DLRVCHGS(RVNO)
              MOVE RSVENTX-DLRVCHGS-FG(RVNO)
                                TO PURFLD-DLRVCHGS-FG(RVNO)
              PERFORM COMPUTE-DLRVCHGS
              MOVE "SNNN062  0." TO VDU
              MOVE RSV-ELE TO VDUROW
              MOVE PURFLD-DLRVCHGS(RVNO) TO VDUNUM2
                                         RSVENTX-DLRVCHGS(RVNO)
              PERFORM SCREENIO
              PERFORM RSV-SET-ANTIC
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE.

           GO TO RSV-CREATE-ROUTINE.
      *----------------------------------------------
       RSV-F-HELD.
           IF RSVENTX-LOANTYPE = "I" AND VDUNUM2 NOT = 0
              MOVE RSVENT-DLRSV-SEQ TO PASSWDW-SEQ
              MOVE " TO ALLOW PARTIC ON IB ACCOUNT" TO PASSWDW-PROMPT2
              PERFORM RSVENT-VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "PARTIC. HELD NOT ALLOWED ON IB ACCOUNTS" TO MESS
                 PERFORM SEND-MESS
                 GO TO RSV-ENTER-ELE.
           MOVE RSVENTX-DLPARVHELD TO RSV-HOLDER.
           MOVE VDUNUM2 TO RSVENTX-DLPARVHELD.
           PERFORM RSV-SET-ANTIC.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           IF RSVENTX-DLPARVHELD = RSV-HOLDER
              GO TO RSV-CREATE-ROUTINE.
       CONT-RSV-F-HELD.
           MOVE "S  A010051." TO VDU.
           MOVE "I" TO VDUBUF RSVENTX-DLPARVHELD-FG.
           PERFORM SCREENIO.

           MOVE "E  A010051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO RSV-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-RSV-F-HELD.
           MOVE VDUBUF TO RSVENTX-DLPARVHELD-FG.
           IF NOT (RSVENTX-DLPARVHELD-FG = "I" OR "C")
              PERFORM RSV-I-OR-C
              GO TO CONT-RSV-F-HELD.
           IF RSVENTX-DLPARVHELD-FG = "C"
              MOVE RSVENTX-DLPARVHELD TO PURFLD-DLPARVHELD
              MOVE RSVENTX-DLPARVHELD-FG TO PURFLD-DLPARVHELD-FG
              PERFORM COMPUTE-DLPARVHELD
              MOVE "SNNN062050." TO VDU
              MOVE PURFLD-DLPARVHELD TO VDUNUM2
                                        RSVENTX-DLPARVHELD
              PERFORM SCREENIO
              PERFORM RSV-SET-ANTIC
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE.

           GO TO RSV-CREATE-ROUTINE.
      *----------------------------------------------
       RSV-F-PAID.
           IF RSVENTX-LOANTYPE = "I" AND VDUNUM2 NOT = 0
              MOVE RSVENT-DLRSV-SEQ TO PASSWDW-SEQ
              MOVE " TO ALLOW PARTIC ON IB ACCOUNT" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "PARTIC. PAID NOT ALLOWED ON IB ACCOUNTS" TO MESS
                 PERFORM SEND-MESS
                 GO TO RSV-ENTER-ELE.
           MOVE RSVENTX-DLPARVPAID TO RSV-HOLDER.
           MOVE VDUNUM2 TO RSVENTX-DLPARVPAID.
           PERFORM RSV-SET-DLCHECK.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           PERFORM RSV-SET-ANTIC.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           IF RSVENTX-DLPARVPAID = RSV-HOLDER
              GO TO RSV-CREATE-ROUTINE.
       CONT-RSV-F-PAID.
           MOVE "S  A010061." TO VDU.
           MOVE "I" TO VDUBUF RSVENTX-DLPARVPAID-FG.
           PERFORM SCREENIO.

           MOVE "E  A010061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO RSV-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-RSV-F-PAID.
           MOVE VDUBUF TO RSVENTX-DLPARVPAID-FG.
           IF NOT (RSVENTX-DLPARVPAID-FG = "I" OR "C")
              PERFORM RSV-I-OR-C
              GO TO CONT-RSV-F-PAID.
           IF RSVENTX-DLPARVPAID-FG = "C"
              MOVE RSVENTX-DLPARVPAID TO PURFLD-DLPARVPAID
              MOVE RSVENTX-DLPARVPAID-FG TO PURFLD-DLPARVPAID-FG
              PERFORM COMPUTE-DLPARVPAID
              MOVE "SNNN062060." TO VDU
              MOVE PURFLD-DLPARVPAID TO VDUNUM2
                                        RSVENTX-DLPARVPAID
              PERFORM SCREENIO
              PERFORM RSV-SET-DLCHECK
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE
              PERFORM RSV-SET-ANTIC
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE.

           GO TO RSV-CREATE-ROUTINE.
      *----------------------------------------------
       RSV-F-DLDISC.
           MOVE "Y" TO FORCE-ENTERED-DISC.
           MOVE RSVENTX-DLDISC TO RSV-HOLDER.
           MOVE VDUNUM2 TO RSVENTX-DLDISC.
           PERFORM RSV-SET-DLCHECK.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           PERFORM RSV-SET-ANTIC.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           IF RSVENTX-DLDISC = RSV-HOLDER
              GO TO RSV-CREATE-ROUTINE.
       CONT-RSV-F-DLDISC.
           MOVE "S  A010071." TO VDU.
           MOVE "I" TO VDUBUF RSVENTX-DLDISC-FG.
           PERFORM SCREENIO.

           MOVE "E  A010071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO RSV-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-RSV-F-DLDISC.
           MOVE VDUBUF TO RSVENTX-DLDISC-FG.
           IF NOT (RSVENTX-DLDISC-FG = "I" OR "C")
              PERFORM RSV-I-OR-C
              GO TO CONT-RSV-F-DLDISC.
           IF RSVENTX-DLDISC-FG = "C"
              MOVE RSVENTX-DLDISC TO PURFLD-DLDISC
              MOVE RSVENTX-DLDISC-FG TO PURFLD-DLDISC-FG
              PERFORM COMPUTE-DLDISC
              MOVE "SNNN062070." TO VDU
              MOVE PURFLD-DLDISC TO VDUNUM2
                                        RSVENTX-DLDISC
              PERFORM SCREENIO
              PERFORM RSV-SET-DLCHECK
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE
              END-IF
              PERFORM RSV-SET-ANTIC
              IF RSV-ERR-FG NOT = 0
                 GO TO RSV-ENTER-ELE.

           GO TO RSV-CREATE-ROUTINE.

      *----------------------------------------------
       RSV-F-LNCOST.
           MOVE RSVENTX-LNCOST TO RSV-HOLDER.
           MOVE VDUNUM2 TO RSVENTX-LNCOST.
           PERFORM RSV-SET-DLCHECK.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           PERFORM RSV-SET-ANTIC.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           IF RSVENTX-LNCOST = RSV-HOLDER
              GO TO RSV-CREATE-ROUTINE.
       CONT-RSV-F-LNCOST.
           MOVE "S  A010101." TO VDU.
           MOVE "I" TO VDUBUF RSVENTX-LNCOST-FG.
           PERFORM SCREENIO.

           MOVE "E  A010101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO RSV-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-RSV-F-LNCOST.
           MOVE VDUBUF TO RSVENTX-LNCOST-FG.
           IF NOT (RSVENTX-LNCOST-FG = "I")
              PERFORM RSV-I
              GO TO CONT-RSV-F-LNCOST.

           GO TO RSV-CREATE-ROUTINE.

      *----------------------------------------------
       RSV-F-DLCOMM-PP.
           MOVE RSVENTX-DLCOMM-PP-CMSI TO RSV-HOLDER.
           MOVE VDUNUM2 TO RSVENTX-DLCOMM-PP-CMSI.
           PERFORM RSV-SET-DLCHECK.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           PERFORM RSV-SET-ANTIC.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           IF RSVENTX-DLCOMM-PP-CMSI = RSV-HOLDER
              GO TO RSV-CREATE-ROUTINE.
       CONT-RSV-F-DLCOMM-PP.
           MOVE "S  A010111." TO VDU.
           MOVE "I" TO VDUBUF RSVENTX-DLCOMM-PP-CMSI-FG.
           PERFORM SCREENIO.

           MOVE "E  A010111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO RSV-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-RSV-F-DLCOMM-PP.
           MOVE VDUBUF TO RSVENTX-DLCOMM-PP-CMSI-FG.
           IF NOT (RSVENTX-DLCOMM-PP-CMSI-FG = "I")
              PERFORM RSV-I
              GO TO CONT-RSV-F-DLCOMM-PP.

           GO TO RSV-CREATE-ROUTINE.

      *----------------------------------------------
       RSV-F-DLCOMM-O3.
           MOVE RSVENTX-DLCOMM-O3-CMSI TO RSV-HOLDER.
           MOVE VDUNUM2 TO RSVENTX-DLCOMM-O3-CMSI.
           PERFORM RSV-SET-DLCHECK.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           PERFORM RSV-SET-ANTIC.
           IF RSV-ERR-FG NOT = 0
              GO TO RSV-ENTER-ELE.
           IF RSVENTX-DLCOMM-O3-CMSI = RSV-HOLDER
              GO TO RSV-CREATE-ROUTINE.
       CONT-RSV-F-DLCOMM-O3.
           MOVE "S  A010121." TO VDU.
           MOVE "I" TO VDUBUF RSVENTX-DLCOMM-O3-CMSI-FG.
           PERFORM SCREENIO.

           MOVE "E  A010121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO RSV-ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-RSV-F-DLCOMM-O3.
           MOVE VDUBUF TO RSVENTX-DLCOMM-O3-CMSI-FG.
           IF NOT (RSVENTX-DLCOMM-O3-CMSI-FG = "I")
              PERFORM RSV-I
              GO TO CONT-RSV-F-DLCOMM-O3.

           GO TO RSV-CREATE-ROUTINE.

       END-RSV-ENTRY-MODULE.

       EXIT-RSV-ENTRY-MODULE.
           EXIT.

      ********************************************
       RSV-DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE RSVENTX-ANTICERN TO RSVANTIC.
           MOVE RSVENTX-PROCEEDS TO RSVPROC.
           MOVE RSVENTX-YLDRATE TO RSVYIELD.

           MOVE RSVENTX-DLRVPRIN(1)    TO RSV01.
           MOVE RSVENTX-DLRVPRIN-FG(1) TO RSV011.
           MOVE RSVENTX-DLRVPRIN(2)    TO RSV02.
           MOVE RSVENTX-DLRVPRIN-FG(2) TO RSV021.
           MOVE RSVENTX-DLRVCHGS(1)    TO RSV03.
           MOVE RSVENTX-DLRVCHGS-FG(1) TO RSV031.
           MOVE RSVENTX-DLRVCHGS(2)    TO RSV04.
           MOVE RSVENTX-DLRVCHGS-FG(2) TO RSV041.
           MOVE RSVENTX-DLPARVHELD     TO RSV05.
           MOVE RSVENTX-DLPARVHELD-FG  TO RSV051.
           MOVE RSVENTX-DLPARVPAID     TO RSV06.
           MOVE RSVENTX-DLPARVPAID-FG  TO RSV061.
           MOVE RSVENTX-DLDISC         TO RSV07.
           MOVE RSVENTX-DLDISC-FG      TO RSV071.
           MOVE RSVENTX-DLRVPRIN(3)    TO RSV08.
           MOVE RSVENTX-DLRVPRIN-FG(3) TO RSV081.
           MOVE RSVENTX-DLRVCHGS(3)    TO RSV09.
           MOVE RSVENTX-DLRVCHGS-FG(3) TO RSV091.
           MOVE RSVENTX-LNCOST         TO RSV10.
           MOVE RSVENTX-LNCOST-FG      TO RSV101.
           MOVE RSVENTX-DLCOMM-PP-CMSI    TO RSV11.
           MOVE RSVENTX-DLCOMM-PP-CMSI-FG TO RSV111.
           MOVE RSVENTX-DLCOMM-O3-CMSI    TO RSV12.
           MOVE RSVENTX-DLCOMM-O3-CMSI-FG TO RSV121.

           MOVE RSV-DISPLAY-BUF TO WR-BUF.
           MOVE RSV-DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM RSV-DISPLAY-FIELDS-T.

      ********************************************
       RSV-DISPLAY-FIELDS-T SECTION.
      ********************************************
           PERFORM RSV-CALC-YIELD.
           MOVE RSVENTX-ANTICERN TO RSVANTIC-T.
           MOVE RSVENTX-PROCEEDS TO RSVPROC-T.
           MOVE RSVENTX-YLDRATE TO RSVYIELD-T.

           MOVE RSV-DISPLAY-BUF-T TO WR-BUF.
           MOVE RSV-DISPLAY-LIST-T TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************
       RSV-SET-ANTIC SECTION.
      ********************************************
           MOVE 0 TO RSV-ERR-FG.
           MOVE RSVENT-ANTICERN TO RSVENTX-ANTICERN.
           IF DL-RVCHGS-CODE(1) = " "
              COMPUTE RSVENTX-ANTICERN = RSVENTX-ANTICERN
                   + RSVENT-DLRVCHGS(1) - RSVENTX-DLRVCHGS(1).
           IF DL-RVCHGS-CODE(2) = " "
              COMPUTE RSVENTX-ANTICERN = RSVENTX-ANTICERN
                   + RSVENT-DLRVCHGS(2) - RSVENTX-DLRVCHGS(2).
           IF DL-RVCHGS-CODE(3) = " "
              COMPUTE RSVENTX-ANTICERN = RSVENTX-ANTICERN
                   + RSVENT-DLRVCHGS(3) - RSVENTX-DLRVCHGS(3).

           IF RSVENTX-LOANTYPE NOT = "I"
              IF DL-PARVHELD-CODE = "B"
                 COMPUTE RSVENTX-ANTICERN = RSVENTX-ANTICERN
                      + RSVENT-DLPARVHELD - RSVENTX-DLPARVHELD
              END-IF
              IF DL-PARVPAID-CODE = " "
                 COMPUTE RSVENTX-ANTICERN = RSVENTX-ANTICERN
                      + RSVENT-DLPARVPAID - RSVENTX-DLPARVPAID.

           COMPUTE RSVENTX-ANTICERN = RSVENTX-ANTICERN
                   - RSVENT-DLDISC + RSVENTX-DLDISC.

           IF NOT (GP-INCL-OTH-IN-EARNED-N  OR
                             GP-INCL-OTH-IN-EARNED-R)
              COMPUTE RSVENTX-ANTICERN = RSVENTX-ANTICERN
                   - RSVENT-LNCOST + RSVENTX-LNCOST.

      * REMOVED 7/2/2019 PT0051 REMOVING PFFILE PARTICIPATING FLATS
      *
      *    IF BR-USE-PARTIC-FLAT-TABLE = "Y"
      *      IF NOT(GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
      *           COMPUTE RSVENTX-ANTICERN = RSVENTX-ANTICERN
      *                - RSVENT-DLPARVHELD + RSVENTX-DLPARVHELD.


           PERFORM RSV-DISPLAY-FIELDS-T.
           IF RSVENTX-ANTICERN < 0
              MOVE "WILL CAUSE NEGATIVE ANTICIPATED EARNINGS!!!"
                                    TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO RSV-ERR-FG.

      ********************************************
       RSV-SET-DLCHECK SECTION.
      ********************************************
           MOVE 0 TO RSV-ERR-FG.
           COMPUTE RSVENTX-PROCEEDS =
               RSVENT-PROCEEDS
                   + RSVENT-DLRVPRIN(1)    - RSVENTX-DLRVPRIN(1)
                   + RSVENT-DLRVPRIN(2)    - RSVENTX-DLRVPRIN(2)
                   + RSVENT-DLRVPRIN(3)    - RSVENTX-DLRVPRIN(3)
                   + RSVENT-DLDISC         - RSVENTX-DLDISC
                   - RSVENT-DLPARVPAID     + RSVENTX-DLPARVPAID
                   + RSVENT-LNCOST         - RSVENTX-LNCOST
                   - RSVENT-DLCOMM-PP-CMSI + RSVENTX-DLCOMM-PP-CMSI
                   - RSVENT-DLCOMM-O3-CMSI + RSVENTX-DLCOMM-O3-CMSI.

           PERFORM RSV-DISPLAY-FIELDS-T.

           IF RSVENTX-PROCEEDS < 0
              MOVE "WILL CAUSE A NEGATIVE DEALER CHECK!!!" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO RSV-ERR-FG.

      ***********************************************
       RSV-I-OR-C SECTION.
      ***********************************************
           MOVE " 'I'-INPUT OR 'C'-CALCULATE!" TO MESS.
           PERFORM SEND-MESS.

      ***********************************************
       RSV-I SECTION.
      ***********************************************
           MOVE " 'I'-INPUT ONLY!" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       RSVENT-VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "RSVENT"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
