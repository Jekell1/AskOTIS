      * COPYMEMBER: LIBLP/OURSPD
      **************************************************************************
      *               (OURSPD)
      *    COMPUTE MEMO, OUR AND NOT OUR CHARGES
      *            AND INS. COMMISSIONS
      *         NOTE:
      *              REMEMBER TO CHANGE 'OURSLN'
      * REV:
      *  JTG 111296 CHANGED PD-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 020620 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS  REGENCY #1801
      *  JTG 030624 CORRECTED CHANGE FOR (MIP) 020620             REGENCY #1801X
      **************************************************************************
       COMPUTE-MEMO-OURS-NOTOURS SECTION.
           MOVE 0 TO SUB
                     NON-MEMO-CHGS
                     NOT-OUR-CHGS
                     OUR-CHGS
                     INS-COMM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF SUB < 9 AND
                    ( NOT ( SUB = 1 AND
                            ( SP-INS-MIP(1) OR
                              SP-INS-MIP(2) OR
                              SP-INS-MIP(3) OR
                              SP-INS-MIP(4)
                            )
                          )
                    )
                    AND
                    ( NOT ( SUB = 2 AND SP-INS-MIP(5)
                          )
                    )
                    AND
                    (
                      NOT (PD-INSOURS(SUB) = "M")
                    )
                    ADD PD-INSPREM(SUB) TO NON-MEMO-CHGS
                    IF PD-INSOURS(SUB) = "N"
                       ADD PD-INSPREM(SUB) TO NOT-OUR-CHGS
                    ELSE
                       ADD PD-INSCOMM(SUB) TO INS-COMM
                       ADD PD-INSPREM(SUB) TO OUR-CHGS
                    END-IF
              END-IF
              IF PD-FEEOURS(SUB) NOT = "M"
                 ADD PD-FEEAMT(SUB) TO NON-MEMO-CHGS
                 IF PD-FEEOURS(SUB) = "N"
                    ADD PD-FEEAMT(SUB) TO NOT-OUR-CHGS
                 ELSE
                    ADD PD-FEEAMT(SUB) TO OUR-CHGS
                 END-IF
              END-IF
           END-PERFORM.

       COMPUTE-MEMO-OURS-NOTOURS-EXIT.
           EXIT.

