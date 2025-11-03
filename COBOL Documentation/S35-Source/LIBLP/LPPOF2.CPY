      * COPYMEMBER: LIBLP/LPPOF2
      ************************************************************************
      *         COMPUTE ADDON INTEREST REBATE FIGURES FOR A LOAN
      *                (AS OF A CURRENT DATE)
      *
      *   NAME: LPPOF2
      *   DESC: THIS ROUTINE IS USED TO COMPUTE THE
      *         REBATE FIGURES FOR LOAN ADDON CANCEL/PAYOFF. IT WILL
      *         YIELD THE FOLLOWING FIGURES:
      *
      *         ROUTINE AUTO COPIES 'LIBLP/ADDONRB'
      *         ------------------------------------------
      *
      *   IN  : POFF-PAYDATE            DATE OF PAYMENT
      *         POFF-LPTRCD             TO REFLECT THE TYPE OF PAYOFF
      *
      *         ----------------        THESE FIELDS ARE USED BY LONPG7.C
      *                                 TO DETERMINE WHICH ADDON RECORDS
      *                                 TO INCLUDE IN THE COMPUTATION OF
      *                                 THE INTEREST REFUND FOR A CANCEL
      *         POFF-INSEFF-TEST
      *         POFF-INSEXP-TEST
      *                                 SET THEM TO - ZERO - OTHERWISE
      *         ----------------
      *
      *   OUT : POFF-INT-REB-TBL(1)     CL ADDON INTEREST REBATE.
      *                         (2)     AH ADDON INTEREST REBATE.
      *                         (3)     PP ADDON INTEREST REBATE.
      *                         (4)     O1 ADDON INTEREST REBATE.
      *                         (5)     O2 ADDON INTEREST REBATE.
      *                         (6)     O3 ADDON INTEREST REBATE.
      *                         (7)     O4 ADDON INTEREST REBATE.
      *                         (8)     O5 ADDON INTEREST REBATE.
      *
      *   USED:
      *   COPY:
      *
      *REV :
      * MJD 052097 ADDED LEVEL TO POFF-INT-ADDON-TBL FOR FEES.
      * JTG 061897 ADDED LOGIC TO TEST FOR ONLY ADDON RECORDS THAT
      *            ARE PART OF THE CANCEL
      * JTG 092697 CHANGED TO PERFORM 'LIBLP/ADDONRB' COMMON LOGIC
      *            TO COMPUTE APR RATE FOR ACTUARIAL REBATE FORMULAS
      * JTG 030212 ADDONS CANCELLED VIA ADDON MODULE DO INCORRECT ADDITIONAL
      *            REFUND ON THE 'LT' INTEREST
      *
      *            CHANGED VALUES OF LT-INS-REBATE FLAG SUCH THAT WHEN:
      *              LT-INS-REBATE = " "  NO REFUND OR CANCEL OCCURED
      *                            = "Y"  INS. WAS CANCELLED VIA ADDON POSTING
      *                            = "R"  INS. WAS REBATED   VIA TRANS POSTING
      *                                                               MULL #382
      *  160113 BAH SPLIT OUT LTFILE, LTIFILE, RENAMED LT- LTI- PD0003
      *  100102  CS GLOBAL LTIFILE
      ************************************************************************
       PAYOFF-INT-ADDONS SECTION.
           PERFORM OPEN-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
                                   QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.
       PAYOFF-INT-ADDONS-NEXT.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (LTI-BRNO NOT = LTI-PATH-OWNBR)
              GO TO END-PAYOFF-INT-ADDONS.

           IF LTI-ACCTNO NOT = LN-ACCTNO
              GO TO END-PAYOFF-INT-ADDONS.

      * THIS LOGIC IS USED BY LONPG7.C
      *----------------------------------------------------------------
           IF NOT (POFF-INSEFF-TEST = 0 AND POFF-INSEXP-TEST = 0)
              IF NOT ( LTI-INSEFF-DATE = POFF-INSEFF-TEST AND
                       LTI-INSEXP-DATE = POFF-INSEXP-TEST
                     )
                 GO TO PAYOFF-INT-ADDONS-NEXT
              END-IF
              IF LTI-INS-REBATE NOT = " "
                 GO TO PAYOFF-INT-ADDONS-NEXT
              END-IF
              PERFORM PAYOFF-INT-ADDONS-GET-LP-REC
              IF POFF-RTNCD = "Y"
                 GO TO PAYOFF-INT-ADDONS-NEXT.
      *----------------------------------------------------------------

      * COMMON LOGIC TO COMPUTE ADDON INTEREST REFUND:
           PERFORM ADDONRB-GET-INT-REBATE.

           IF REB-SUB NOT = 0
              MULTIPLY POFF-ONE BY REB-REBATE
              ADD REB-REBATE TO POFF-ADDON-INT-REBATE
           ELSE
              MOVE 0 TO REB-REBATE.
           IF LTI-INS-TYPE = 0
              ADD REB-REBATE TO POFF-INT-REB-TBL(9)
           ELSE
              ADD REB-REBATE TO POFF-INT-REB-TBL(LTI-INS-TYPE).
           GO TO PAYOFF-INT-ADDONS-NEXT.

       END-PAYOFF-INT-ADDONS.
           PERFORM CLOSE-LTI1-FILE.

       COPY "LIBLP/ADDONRB.CPY".

      ****************************************************
      *         PAYOFF-INT-ADDONS-GET-LP-REC
      *
      *    DUMMY SECTION USED BY LONPG7.C
      *    IT MUST BE THE LAST LINE OF THIS COPY MEMBER
      *    TO ALLOW LONPG7.C TO PLACE ITS LOGIC TO READ
      *    PAYMENT RECORDS.
      ****************************************************
       PAYOFF-INT-ADDONS-GET-LP-REC SECTION.

