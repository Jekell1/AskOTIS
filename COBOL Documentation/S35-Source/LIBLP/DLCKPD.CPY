      * COPYMEMBER: LIBLP/DLCKPD
      **********************************************************************
      *                  (DLCKPD)
      *    ROUTINE FOR SALES FINANCE
      *    CALCULATE DEALER CHECK
      *         NOTE:
      *              REMEMBER TO CHANGE 'DLCKLN'
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
      *  JTG 020909 ADDED PD-DLCOMM-O3-CMSI PD-DLCOMM-PP-CMSI  REGACC #1846
      *  JTG 030619 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *             RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *             AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      **************************************************************************
       COMPUTE-DEALER-CHECK SECTION.
           IF PD-SERVACCT = "Y"
              MOVE 0 TO PD-PROCEEDS
                        TEST-PROCEEDS
              GO TO EXIT-COMPUTE-DEALER-CHECK.

           COMPUTE PD-PROCEEDS
                   TEST-PROCEEDS =
                     PD-LNAMT - PD-DLRVPRIN(1) - PD-DLRVPRIN(2)
                              - PD-DLRVPRIN(3)
                              - PD-ANTICERN(1) - PD-ANTICERN(2)
                              - PD-ANTICERN(3) - PD-ANTICERN(4)
                              - PD-ANTICERN(5)
      *JTG                    - PD-ANTICERN(6)   PERSP-LNCOST-FRMLA 'D'
                              - OUR-CHGS
                              - PD-PRBAL       - PD-PRINTDUE
                              + PD-PRFINCHGRB  + PD-PRINSRB.
           IF SP-LNCOST-FRMLA = "D"
              SUBTRACT PD-ANTICERN(6) FROM PD-PROCEEDS
                                           TEST-PROCEEDS.
           IF DL-RVCHGS-CODE(1) = " "
              SUBTRACT PD-DLRVCHGS(1) FROM PD-PROCEEDS
                                          TEST-PROCEEDS.
           IF DL-RVCHGS-CODE(2) = " "
              SUBTRACT PD-DLRVCHGS(2) FROM PD-PROCEEDS
                                          TEST-PROCEEDS.
           IF DL-RVCHGS-CODE(3) = " "
              SUBTRACT PD-DLRVCHGS(3) FROM PD-PROCEEDS
                                          TEST-PROCEEDS.
      *JTG.AC
           IF PD-LOANTYPE = "P"
              IF DL-PARVHELD-CODE = "B"
                 SUBTRACT PD-DLPARVHELD FROM PD-PROCEEDS
                                             TEST-PROCEEDS.
      *JTG.AC

           IF PD-LOANTYPE = "P"
              IF DL-PARVPAID-CODE = "A" OR "C"
                 ADD PD-DLPARVPAID TO PD-PROCEEDS
                                      TEST-PROCEEDS.
           IF PD-LOANTYPE = "I"
              IF DL-PARVPAID-CODE-IB = " " OR "C" OR "D"
                 ADD PD-DLPARVPAID TO PD-PROCEEDS
                                      TEST-PROCEEDS.
      * RELIANCE = "B"
      * BRINGS UP WINDOW NO MATTER WHAT. SO WE MUST ALWAYS ADD
      * THE DLPROC TO THE PROCEEDS, BUT FIRST SUBTRACT OUT NOT-OURS !!
           IF BR-SFDLO3-WINDOW-FG = "B"
              IF PD-INSOURS(6) = "N"
                 SUBTRACT PD-INSPREM(6) FROM PD-PROCEEDS TEST-PROCEEDS.

           ADD PD-O3INS-DLPROC TO PD-PROCEEDS
                                  TEST-PROCEEDS.

      *  JTG 020909                                            REGACC #1846
           ADD PD-DLCOMM-O3-CMSI PD-DLCOMM-PP-CMSI TO PD-PROCEEDS
                                                      TEST-PROCEEDS.
       EXIT-COMPUTE-DEALER-CHECK.
           EXIT.

