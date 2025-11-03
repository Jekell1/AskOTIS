      * COPYMEMBER: LIBLP/DLCKLN
      **********************************************************************
      *                  (DLCKLN)
      *    ROUTINE FOR SALES FINANCE
      *    CALCULATE DEALER CHECK
      *         NOTE:
      *              REMEMBER TO CHANGE 'DLCKPD'
      * REV:
      *  BAH 022796 FOR SFDLO3-WINDOW-FG = "B", ADJUST DEALER CHECK
      *             ACCORDINGLY (RELIANCE)
      *  JTG 071797 ADDED LOGIC FOR DL-PARVPAID-CODE = "A" OR "C"
      *  JTG 102098 ADDED DL-PARVPAID-CODE-IB "C" INSTANT AUTO #744
      *  JTG 102698 CORRECTED ABOVE CHANGE #744 WHICH LOST REDUCTION
      *             OF DLCK WITH PRIOR LOAN INFO
      *  JTG 990310 ALLOWED PARTICIPATION HELD ON IB ACCOUNTS, AUTOMOTIVE CREDIT
      *  JTG 990317 ADDED SP-DLDISC-RVPRIN1-PCNT,                       TFC #008
      *                   SP-LNCOST-FRMLA-VALID VALUE "D"
      *  JTG 020909 ADDED LN-DLCOMM-O3-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
      *  JTG 030619 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *             RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *             AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      **************************************************************************
       COMPUTE-DEALER-CHECK SECTION.
           IF LN-SERVACCT = "Y"
              MOVE 0 TO LN-PROCEEDS
                         TEST-PROCEEDS
              GO TO EXIT-COMPUTE-DEALER-CHECK.

           COMPUTE LN-PROCEEDS
                   TEST-PROCEEDS =
              LN-LNAMT - LN-DLRVPRIN(1) - LN-DLRVPRIN(2)
                       - LN-DLRVPRIN(3)
                       - LN-ANTICERN(1) - LN-ANTICERN(2)
                       - LN-ANTICERN(3) - LN-ANTICERN(4)
                       - LN-ANTICERN(5)
      *JTG             - PD-ANTICERN(6)   PERSP-LNCOST-FRMLA 'D'
                       - OUR-CHGS
                       - LN-PRBAL       - LN-PRINTDUE
                       + LN-PRFINCHGRB  + LN-PRINSRB.
           IF SP-LNCOST-FRMLA = "D"
              SUBTRACT LN-ANTICERN(6) FROM LN-PROCEEDS
                                           TEST-PROCEEDS.
           IF DL-RVCHGS-CODE(1) = " "
              SUBTRACT LN-DLRVCHGS(1) FROM LN-PROCEEDS
                                           TEST-PROCEEDS.
           IF DL-RVCHGS-CODE(2) = " "
              SUBTRACT LN-DLRVCHGS(2) FROM LN-PROCEEDS
                                           TEST-PROCEEDS.
           IF DL-RVCHGS-CODE(3) = " "
              SUBTRACT LN-DLRVCHGS(3) FROM LN-PROCEEDS
                                           TEST-PROCEEDS.
           IF LN-LOANTYPE = "P"
              IF DL-PARVHELD-CODE = "B"
                 SUBTRACT LN-DLPARVHELD FROM LN-PROCEEDS
                                             TEST-PROCEEDS.
           IF LN-LOANTYPE = "P"
              IF DL-PARVPAID-CODE = "A" OR "C"
                 ADD LN-DLPARVPAID TO LN-PROCEEDS
                                      TEST-PROCEEDS.
           IF LN-LOANTYPE = "I"
              IF DL-PARVPAID-CODE-IB = " " OR "C" OR "D"
                 ADD LN-DLPARVPAID TO LN-PROCEEDS
                                      TEST-PROCEEDS.
      * RELIANCE = "B"
      * BRINGS UP WINDOW NO MATTER WHAT. SO WE MUST ALWAYS ADD
      * THE DLPROC TO THE PROCEEDS, BUT FIRST SUBTRACT OUT NOT-OURS !!
           IF BR-SFDLO3-WINDOW-FG = "B"
              IF LN-INSOURS(6) = "N"
                 SUBTRACT LN-INSPREM(6) FROM LN-PROCEEDS TEST-PROCEEDS.

           ADD LN-O3INS-DLPROC TO LN-PROCEEDS
                                  TEST-PROCEEDS.
      *  JTG 020909                                            REGACC #1846
           ADD LN-DLCOMM-O3-CMSI LN-DLCOMM-PP-CMSI TO LN-PROCEEDS
                                                      TEST-PROCEEDS.

       EXIT-COMPUTE-DEALER-CHECK.
           EXIT.

