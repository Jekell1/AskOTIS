      **********************************************
      * /USR/DATA/R1/FTP/OUT/PAYS/TR.BRNO.NUMBER.SEQ
      *                              1234.123456.123
      * THERE IS A PROCESS SCANNING THIS DIRECTORY TO SEND
      * THESE IMMEDIATELY TO REPAY 
      *
      * REV:
      * 2023.1218 MOVE TP-BRNO TO REPAY PATH BRNO, MUST BE OWNING BRANCH
      *           FOR INTERBRANCH PAYMENTS #1629A
      * 2024.0214 MOVE TP-REPAY-TRANS-ID TO XTRP-REPAY-TRANS-ID. #1641
      * 2025.0126 ADDED XTRP-CLOSFEE-REB-AMT #1697 S35Q-157
      **********************************************
       UPDATE-REPAY-TRP-FILE SECTION.
      **********************************************
           MOVE "UPDATING REPAY TRP FILE.........." TO MESS.
           PERFORM SEND-LEGEND.

      * USR/DATA/R1/
           MOVE FIL-BASE  TO REPAY-TRP-PATH-BASE.
           CALL "C$TOLOWER" USING REPAY-TRP-PATH-DIR,VALUE 8.
           MOVE TP-BRNO   TO REPAY-TRP-PATH-BRNO.
           MOVE LN-ACCTNO TO REPAY-TRP-PATH-NUMBER.
           MOVE TP-LINE   TO REPAY-TRP-PATH-SEQ.

           MOVE REPAY-TRP-DIR TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO EXIT-UPDATE-REPAY-TRP-FILE.

           PERFORM OPEN-REPAY-TRP-FILE-OUTPUT.

           MOVE SPACES TO XTRP-REC.

           MOVE TP-BRNO                  TO XTRP-BRNO WS-BRNO.                 
           MOVE TP-ACCTNO                TO XTRP-ACCTNO WS-NUMBER.
           MOVE WS-BRACCTNO              TO XTRP-BRACCTNO.
           MOVE BR-REGION-NUMBER         TO XTRP-REGION-NUMBER.
    
           MOVE TP-DATE                  TO XTRP-TRANS-DATE.
           MOVE "P"                      TO XTRP-CODE.
           MOVE TP-CLASS                 TO XTRP-CLASS.
           MOVE TP-LINE                  TO XTRP-LINE
                                            XTRP-LINE-3.
           MOVE TP-LAWCODE               TO XTRP-LAWCODE.
           MOVE TP-JDCD                  TO XTRP-JDCD.
           MOVE TP-PLCD                  TO XTRP-PLCD.
           MOVE SPACE                    TO XTRP-MORELN.
           MOVE TP-SSNO                  TO XTRP-SSNO.
           MOVE TP-MAKER                 TO XTRP-MAKER.
           MOVE TP-IBPC                  TO XTRP-IBPC.
           MOVE TP-TRCD                  TO XTRP-TRCD.
           MOVE TP-REFNO                 TO XTRP-REFNO.
           MOVE TP-PAYDATE               TO XTRP-PAYDATE.
           MOVE TP-DUEDATE               TO XTRP-DUEDATE.
           MOVE TP-TRAMT                 TO XTRP-TRAMT.
           INSPECT XTRP-TRAMT REPLACING LEADING "+" BY " ".
           MOVE TP-INTDUE                TO XTRP-INTDUE.
           INSPECT XTRP-INTDUE REPLACING LEADING "+" BY " ".
           MOVE TP-APOTH                 TO XTRP-APOTH.
           INSPECT XTRP-APOTH REPLACING LEADING "+" BY " ".
           MOVE TP-APOT2                 TO XTRP-APOT2.
           INSPECT XTRP-APOT2 REPLACING LEADING "+" BY " ".
           MOVE TP-APINT                 TO XTRP-APINT.
           INSPECT XTRP-APINT REPLACING LEADING "+" BY " ".
           MOVE TP-APLC                  TO XTRP-APLC.
           INSPECT XTRP-APLC REPLACING LEADING "+" BY " ".
           MOVE TP-APCUR                 TO XTRP-APCUR.
           INSPECT XTRP-APCUR REPLACING LEADING "+" BY " ".

           PERFORM VARYING REPAY-TRP-SUB FROM 1 BY 1
                                         UNTIL REPAY-TRP-SUB > 3
              MOVE TP-GLNO(REPAY-TRP-SUB)          TO XTRP-GLNO(SUB)
              MOVE TP-GLAMT(REPAY-TRP-SUB)         TO XTRP-GLAMT(SUB)
              INSPECT XTRP-GLAMT(REPAY-TRP-SUB)
                                         REPLACING LEADING "+" BY " "
           END-PERFORM.

           PERFORM VARYING REPAY-TRP-SUB FROM 1 BY 1
                                         UNTIL REPAY-TRP-SUB > 6
              MOVE TP-EARNINGS(REPAY-TRP-SUB) TO XTRP-EARNINGS(SUB)
              INSPECT XTRP-EARNINGS(REPAY-TRP-SUB)
                                         REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE TP-OTHBAL                TO XTRP-OTHBAL.
           INSPECT XTRP-OTHBAL REPLACING LEADING "+" BY " ".
           MOVE TP-OT2BAL                TO XTRP-OT2BAL.
           INSPECT XTRP-OT2BAL REPLACING LEADING "+" BY " ".
           MOVE TP-INTBAL                TO XTRP-INTBAL.
           INSPECT XTRP-INTBAL REPLACING LEADING "+" BY " ".
           MOVE TP-LCBAL                 TO XTRP-LCBAL.
           INSPECT XTRP-LCBAL REPLACING LEADING "+" BY " ".
           MOVE TP-CURBAL                TO XTRP-CURBAL.
           INSPECT XTRP-CURBAL REPLACING LEADING "+" BY " ".

           MOVE TP-PDTH-DATE             TO XTRP-PDTHRU.

           MOVE TP-DLNO                  TO XTRP-DLNO.
           MOVE TP-DLPROC                TO XTRP-DLPROC.
           INSPECT XTRP-DLPROC REPLACING LEADING "+" BY " ".
           MOVE TP-SERVACCT              TO XTRP-SERVACCT.
           MOVE TP-SALEFIN               TO XTRP-SALEFIN.
           MOVE TP-INSURED               TO XTRP-INSURED.
           MOVE TP-INSOURS               TO XTRP-INSOURS.
           MOVE TP-LOANTYPE              TO XTRP-LOANTYPE.
      * TP-TILLNO IS 9(2) IN A30. XTP-TILL IS STILL PIC X LIKE A15.
      *    MOVE TP-TILLNO                TO  XTRP-TILLNO.
           MOVE TP-TILLNO                TO WS-TILLNO.
           MOVE WS-TILLNO-X              TO XTRP-TILLNO.
           MOVE TP-LCPDTH-DATE           TO XTRP-LCPD.
           MOVE TP-DLNO                  TO XTRP-DLNO.
           MOVE TP-WORKAMT(1)            TO XTRP-WORKAMT1.
           INSPECT XTRP-WORKAMT1 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(2)            TO XTRP-WORKAMT2.
           INSPECT XTRP-WORKAMT2 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(3)            TO XTRP-WORKAMT3.
           INSPECT XTRP-WORKAMT3 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(4)            TO XTRP-WORKAMT4.
           INSPECT XTRP-WORKAMT4 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(5)            TO XTRP-WORKAMT5.
           INSPECT XTRP-WORKAMT5 REPLACING LEADING "+" BY " ".
           MOVE TP-WORKAMT(6)            TO XTRP-WORKAMT6.
           INSPECT XTRP-WORKAMT6 REPLACING LEADING "+" BY " ".
           MOVE TP-POSTING-BR            TO XTRP-POSTING-BR.
           MOVE TP-OTHBR-FG              TO XTRP-OTHBR-FG.
           MOVE TP-REV                   TO XTRP-REV.
           MOVE TP-REVTRCD               TO XTRP-REVTRCD.
           MOVE TP-REVSEQNO              TO XTRP-REVSEQNO.
           MOVE TP-STATUSFG              TO XTRP-STATUSFG.
           MOVE TP-KEEPAPINT             TO XTRP-KEEPAPINT.
           MOVE TP-INTPAID               TO XTRP-INTPAID.
           INSPECT XTRP-INTPAID REPLACING LEADING "+" BY " ".
           MOVE TP-PRIOR-POOLID          TO XTRP-PRIOR-POOLID.
           MOVE TP-INSTYPES              TO XTRP-INSTYPES.
      *    MOVE TP-DLRVPRIN1             TO XTRP-DLRVPRIN1.
           MOVE 0                        TO XTRP-DLRVPRIN1.
           INSPECT XTRP-DLRVPRIN1 REPLACING LEADING "+" BY " ".
      *    MOVE TP-DLPARVPAID            TO XTRP-DLPARVPAID.
           MOVE 0                        TO XTRP-DLPARVPAID.
           INSPECT XTRP-DLPARVPAID REPLACING LEADING "+" BY " ".
      *    MOVE TP-DLPARVHELD            TO XTRP-DLPARVHELD.
           MOVE 0                        TO XTRP-DLPARVHELD.
           INSPECT XTRP-DLPARVHELD REPLACING LEADING "+" BY " ".
           MOVE TP-DLPARVHELD-ADJ        TO XTRP-DLPARVHELD-ADJ.
           INSPECT XTRP-DLPARVHELD-ADJ REPLACING LEADING "+" BY " ".
           MOVE TP-DLPARVPAID-ADJ        TO XTRP-DLPARVPAID-ADJ.
           INSPECT XTRP-DLPARVPAID-ADJ REPLACING LEADING "+" BY " ".
           MOVE TP-NEWSTAT-RATE          TO XTRP-NEWSTAT-RATE.
           MOVE TP-POCD                  TO XTRP-POCD.
           MOVE TP-PODATE                TO XTRP-PODATE.
           MOVE TP-DL-RVCHGS-CODE(1)     TO XTRP-DL-RVCHGS-CODE1.
           MOVE TP-DL-RVCHGS-CODE(2)     TO XTRP-DL-RVCHGS-CODE2.
           MOVE TP-DL-RVCHGS-CODE(3)     TO XTRP-DL-RVCHGS-CODE3.
           MOVE TP-DL-PARVHELD-CODE      TO XTRP-DL-PARVHELD-CODE.
           MOVE TP-DL-PARVPAID-CODE      TO XTRP-DL-PARVPAID-CODE.
           MOVE TP-TOTNODEF              TO XTRP-TOTNODEF.
           INSPECT XTRP-TOTNODEF REPLACING LEADING "+" BY " ".
           MOVE TP-LNAMT                 TO XTRP-LNAMT.
           INSPECT XTRP-LNAMT REPLACING LEADING "+" BY " ".
           MOVE TP-ORIG-1STPYDATE        TO XTRP-ORIG-1STPAY.
           MOVE TP-OLD1ST-PYDATE         TO XTRP-OLD1ST-PYDATE.
           MOVE TP-1STPYDATE             TO XTRP-1STPYDATE.
           MOVE TP-ORGTERM               TO XTRP-ORGTERM.
           MOVE TP-INSCOMM               TO XTRP-INSCOMM.
           INSPECT XTRP-INSCOMM REPLACING LEADING "+" BY " ".
           MOVE TP-MANUAL-RB             TO XTRP-MANUAL-RB.
           MOVE TP-USERID                TO XTRP-USERID.
           MOVE TP-POSTTIME              TO XTRP-POSTTIME.
           MOVE TP-PLTYPE                TO XTRP-PLTYPE.
           MOVE TP-PL-OTH-AR             TO XTRP-PL-OTH-AR.
           INSPECT XTRP-PL-OTH-AR REPLACING LEADING "+" BY " ".
           MOVE TP-LP-DLNO               TO XTRP-LP-DLNO.
           MOVE TP-REPOCD                TO XTRP-REPOCD.
           MOVE TP-SPRCLASS              TO XTRP-SPRCLASS.
           MOVE TP-MAX-DEFER-FG          TO XTRP-MAX-DEFER-FG.
           MOVE TP-POSTDATE              TO XTRP-POSTDATE.
           MOVE TP-UNITPER-CD            TO XTRP-UNITPER-CD.
           MOVE TP-UNITPER-FREQ          TO XTRP-UNITPER-FREQ.
           MOVE "N"                      TO XTRP-REVOLVING.

           PERFORM VARYING REPAY-TRP-SUB FROM 1 BY 1
                                         UNTIL REPAY-TRP-SUB > 4
              MOVE 0                     TO XTRP-RINSPREM(REPAY-TRP-SUB)
              INSPECT XTRP-RINSPREM(REPAY-TRP-SUB)
                                         REPLACING LEADING "+" BY " "
              MOVE 0                     TO XTRP-RINSCOMM(REPAY-TRP-SUB)
              INSPECT XTRP-RINSCOMM(REPAY-TRP-SUB)
                                         REPLACING LEADING "+" BY " "
           END-PERFORM.

           MOVE 0                        TO XTRP-APOTH-MEMBERSHIP.
           INSPECT XTRP-APOTH-MEMBERSHIP REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-NSF.
           INSPECT XTRP-APOTH-NSF REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC1.
           INSPECT XTRP-APOTH-MISC1 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC2.
           INSPECT XTRP-APOTH-MISC2 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC3.
           INSPECT XTRP-APOTH-MISC3 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC4.
           INSPECT XTRP-APOTH-MISC4 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC5.
           INSPECT XTRP-APOTH-MISC5 REPLACING LEADING "+" BY " ".
           MOVE 0                        TO XTRP-APOTH-MISC6.
           INSPECT XTRP-APOTH-MISC6 REPLACING LEADING "+" BY " ".
           MOVE TP-SP-DEFFRMLA           TO XTRP-SP-DEFFRMLA.
           MOVE TP-COLLECTOR             TO XTRP-COLLECTOR.
           MOVE TP-PENALTY-INT           TO XTRP-PENALTY-INT.
           INSPECT XTRP-PENALTY-INT REPLACING LEADING "+" BY " ".
           MOVE TP-INSCOMP               TO XTRP-INSCOMP.
           MOVE TP-PROMDATE              TO XTRP-PROMDATE.
           MOVE TP-PROMAMT               TO XTRP-PROMAMT.
           INSPECT XTRP-PROMAMT REPLACING LEADING "+" BY " ".
           MOVE TP-PROMID                TO XTRP-PROMID.

      * UPDATED FIELDS, 10/15/02, BLV:
           MOVE TP-INS-TAX-RB            TO XTRP-INS-TAX-RB.
           INSPECT XTRP-INS-TAX-RB REPLACING LEADING "+" BY " ".
           MOVE TP-PROMDAYS-PASTDUE      TO XTRP-PROMDAYS-PASTDUE.
           INSPECT XTRP-PROMDAYS-PASTDUE REPLACING LEADING "+" BY " ".

           MOVE TP-MIP-FG                TO XTRP-MIP-FG.
           MOVE TP-MIP-CLPAID            TO XTRP-MIP-CLPAID.
           INSPECT XTRP-MIP-CLPAID REPLACING LEADING "+" BY " ".
           MOVE TP-MIP-CLCOMM            TO XTRP-MIP-CLCOMM.
           INSPECT XTRP-MIP-CLCOMM REPLACING LEADING "+" BY " ".
           MOVE TP-MIP-AHPAID            TO XTRP-MIP-AHPAID.
           INSPECT XTRP-MIP-AHPAID REPLACING LEADING "+" BY " ".
           MOVE TP-MIP-AHCOMM            TO XTRP-MIP-AHCOMM.
           INSPECT XTRP-MIP-AHCOMM REPLACING LEADING "+" BY " ".
           MOVE TP-MIP-CLPDTH-DATE       TO XTRP-MIP-CLPDTH.
           MOVE TP-MIP-AHPDTH-DATE       TO XTRP-MIP-AHPDTH.
           MOVE TP-LP-SEQNO                 TO XTRP-LP-SEQNO.

           MOVE TP-PRIOR-DF-CODE            TO XTRP-PRIOR-DF-CODE.

           IF TP-PRIOR-DF-POSTDATE = 20202020 OR 02020202
              MOVE 0 TO XTRP-PRIOR-DF-POSTDATE
           ELSE
              MOVE TP-PRIOR-DF-POSTDATE TO XTRP-PRIOR-DF-POSTDATE.

           MOVE TP-ACTIONCD2             TO XTRP-ACTIONCD2.
           MOVE TP-LCDUE                 TO XTRP-LCDUE.
           INSPECT XTRP-LCDUE REPLACING LEADING "+" BY " ".

           MOVE TP-REPAY-TRANS-ID        TO XTRP-REPAY-TRANS-ID.
           MOVE TP-CLOSFEE-REB-AMT       TO XTRP-CLOSFEE-REB-AMT.
           INSPECT XTRP-CLOSFEE-REB-AMT REPLACING LEADING "+" BY " ".

           MOVE X"0D"                    TO XTRP-CR.
           MOVE X"0A"                    TO XTRP-LF.
              
           MOVE XTRP-REC TO REPAY-TRP-REC.
           PERFORM WRITE-REPAY-TRP-FILE.
      *    IF IO-FG = 9
      *       GO TO IO-ERROR.

           PERFORM CLOSE-REPAY-TRP-FILE.

       EXIT-UPDATE-REPAY-TRP-FILE.
           EXIT.

