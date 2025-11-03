       PAID-THRU-ADDON-SUM SECTION.
           IF LTP-SCHLD-TERM(PDTH) = 0
              MOVE 17 TO PDTH
              GO TO PAID-THRU-ADDON-SUM-EXIT.

           MOVE LTP-SCHLD-PAYMNT(PDTH) TO PDTH-CUR-SCHD-PAY
                                          PDTH-NXT-SCHD-PAY
           COMPUTE PDTH-WORKER =
              LTP-SCHLD-PAYMNT(PDTH) * LTP-SCHLD-TERM(PDTH).
           IF PDTH-PAYMNTD > PDTH-WORKER
              ADD LTP-SCHLD-TERM(PDTH) TO NDTE-HOLD
              SUBTRACT PDTH-WORKER FROM PDTH-PAYMNTD
              GO TO PAID-THRU-ADDON-SUM-EXIT.

           MOVE 0 TO PDTH-CNT.
       PAID-THRU-ADDON-NEXT.
           IF PDTH-CNT < LTP-SCHLD-TERM(PDTH)
              IF PDTH-PAYMNTD > LTP-SCHLD-PAYMNT(PDTH)
                 SUBTRACT LTP-SCHLD-PAYMNT(PDTH) FROM PDTH-PAYMNTD
                 ADD 1 TO NDTE-HOLD PDTH-CNT
                 GO TO PAID-THRU-ADDON-NEXT
              ELSE
                 COMPUTE PDTH-PAYMENTS ROUNDED =
                    PDTH-PAYMNTD / LTP-SCHLD-PAYMNT(PDTH)
                 ADD PDTH-PAYS TO NDTE-HOLD
                 MOVE 0 TO PDTH-PAYMNTD
                 IF PDTH-REMAIN = 0 AND
                    LTP-SCHLD-TERM(PDTH) = (PDTH-CNT + 1)
                    MOVE LTP-SCHLD-PAYMNT(PDTH + 1) TO PDTH-CUR-SCHD-PAY
                                                      PDTH-NXT-SCHD-PAY
                 ELSE
                    IF LTP-SCHLD-TERM(PDTH) = (PDTH-CNT + 1)
                       MOVE LTP-SCHLD-PAYMNT(PDTH + 1) TO
                                                      PDTH-NXT-SCHD-PAY
                    ELSE
                       NEXT SENTENCE
           ELSE
              MOVE 0 TO PDTH-PAYMNTD.

       PAID-THRU-ADDON-SUM-EXIT.
           ADD 1 TO PDTH.

      *================================================================*
      * END COPYBOOK: LIBGB\SPPDTH.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLCHG.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLCHG
      ***********************************************************************
      *   NAME: LPLCHG - LATE CHARGE CALCULATION
      *   DESC: THIS ROUTINE DETERMINES HOW MUCH
      *         TO CHARGE WHEN A LATE CHARGE IS
      *         ASSESED FOR A DELINQ.PAYMENT.
      *   IN  : LCHG-LATE (WHEN LCTYPE = C),
      *                   ITH DOLLARS LATE
      *         LCHG-LCPDTH-DATE (ADDED 070187 AMW)
      *         LCHG-CURPAY  CURRENT PAYMENT DUE IF USING PMT SCHD
      *                      OR SP-LCTYPE = 'M'.
      *         LCHG-DAYS-AFTER-DUE (ADDED 030399 GLF)
      *   OUT : LCHG-CHARGE
      *   USED: LN-REGPYAMT, SP-LCFRMLA, SP-LCTYPE
      *         SP-LCRATE, SP-LCMINCHG, SP-LCMAXAMT, LN-MAINTFEE
      *         LN-1STPYDA LN-1STPYDATE
      *   COPY: LPLCHGW
      *
      * REV:
      *  051286 JTG REDUCED REGPYAMT BY MAINT-FEES
      *  072286 JTG OPENED FORMULA "C" TO THIS ROUTINE
      *  052987 JTG OPENED FORMULA "D" TO THIS ROUTINE
      *  063087 AMW ADD LCTYPE OF E,F,G,H,I.
      *  090689 JTG ADDED LCTYPE 'J'
      *  061290 JTG ADDED LCTYPE 'K' AND 'L' (PACESETTER)
      *  091390 SLC ADDED LCHG-CURPAY, WHICH IS CURRENT PAYMENT IF
      *             LOAN HAS A PAY SCHD RECORD.
      *  060592 JTG ADDED LOGIC FOR LN-LC-CODE (ROSE SHANIS)
      *  062092 JTG ADDED LOGIC FOR LCTYPE 'M' (COLONIAL TN)
      *  080495 KEC REMOVED SP-LC-CODE FROM CALL "CL/ELREAD" USING
      *  111595 BAH ADDED SP-LCTYPE 'N' (MERCURY, TN)
      *  020296 BAH ADDED SP-LCTYPE 'O' (EAST TENNESSEE)
      *  103096 JTG CHANGED TEST ON LN-LC-CODEX TO TEST FOR "00", WHICH
      *             SHOULD ELIMINATE UNNECESSARY IO ON EL-FILE
      *  050797 JTG CHANGED CALL TO ELREAD TO PASS SP-LCUNITPER-CD
      *  122198 JTG ADDED SP-LCTYPE = 'P'  OHIO PRIOR 07/12/96, REGENCY #002
      *  030399 GLF ADDED LOGIC FOR LCHG-DAYS-AFTER-DUE 'Q' (MULLEN)
      *  040331 BAH ADDED SP-LC-ON-JUDGEMENTS FOR RIVERSIDE, PR# 2272, STATE
      *             LAW SAYS NO LATE CHARGES ON JUDGEMENT ACCOUNTS
      *  JTG 050512 ADDED LOGIC TO LET SP-LCUNITPER-CD = "A" WORK
      *             WHEN SP-LCTYPE = 'C', 'K', 'M' OR 'O'       CREATIVE #491
      *  BLM 100219 CHANGED THIS SETTING OF LCHG-WORK TO NOT INCLUDE ESCROW,
      *             CHANGED THIS LOGIC (OLD):
      *
      *    IF SP-LCTYPE = "A" OR "E" OR "F"
      *       IF LN-PAY-SCHLD-FG = "Y"
      *          SUBTRACT LN-MAINTFEE FROM LCHG-CURPAY GIVING LCHG-WORK
      *       ELSE
      *          SUBTRACT LN-MAINTFEE FROM LN-REGPYAMT GIVING LCHG-WORK.
      *                     TO THIS LOGIC (NEW):
      *
      *    IF SP-LCTYPE = "A" OR "E" OR "F"
      *       IF (LN-PAY-SCHLD-FG = "Y" AND LN-ESCROW-FG NOT = "Y" AND
      *                                     LN-ESCROW-FG NOT = "I")
      *          SUBTRACT LN-MAINTFEE FROM LCHG-CURPAY GIVING LCHG-WORK
      *       ELSE
      *          SUBTRACT LN-MAINTFEE FROM LN-REGPYAMT GIVING LCHG-WORK.
      * KEC 190219 REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      *            REMOVED REDEFINE FOR SP-LC-CODEX; UNUSED
      ***********************************************************************
