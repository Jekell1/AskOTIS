      * COPYMEMBER: LIBLP/LPEADJ
      ***************************************************************
      *   NAME: LPEADJ
      *   DESC: PRORATION OF INTEREST REBATE FOR EARNINGS
      *         ADJUSTMENT OF INTEREST, SERVICE CHARGE
      *         AND MAINTENANCE FEES
      *
      *   IN  : EADJ-REBATE         <--  REB-REBATE(7)
      *
      *   OUT : EADJ-ADJUST   (1)   INTEREST
      *                       (2)   NOT USED
      *                       (3)   SER CHG
      *                       (4)   MAINT FEE
      *
      *         EADJ-ADJUST-FG(1)   FLAGS IDENTIFY THAT THIS
      *                       (2)   COMPONENT SHOULD BE ADJUSTED
      *                       (3)
      *                       (4)
      *
      *   REV :
      *   050389 SLC IF THE SUM OF THE ANTICIPATEDS ARE 0 WE
      *              CAN NOT PRORATE SO THE ENTIRE REBATE IS
      *              FORCED TO INTEREST CAUSING NEGATIVE EARNINGS.
      *   081090 JTG STOPPED CONSIDER OF EXTCHG (2), SINCE IT
      *              IS ALWAYS BUNDLED WITH INTEREST AND LEVEL 2
      *              NOW HOLDS ADDON INSURANCE FINCHG INFO.
      ***************************************************************
       EADJ-EARN-ADJUSTMENT SECTION.
           MOVE SPACES TO EADJ-ADJUST-FG(1) EADJ-ADJUST-FG(2)
                          EADJ-ADJUST-FG(3) EADJ-ADJUST-FG(4).
           MOVE ZEROS TO  EADJ-ADJUST(1) EADJ-ADJUST(2)
                          EADJ-ADJUST(3) EADJ-ADJUST(4).

           IF SP-RBFRMLA(7) = "A"
              PERFORM EADJ-EARN-A
           ELSE
              IF SP-RBFRMLA(7) = "D" OR "F"
                 PERFORM EADJ-EARN-3
              ELSE
                 MOVE EADJ-REBATE TO EADJ-ADJUST(1)
                 MOVE "Y" TO EADJ-ADJUST-FG(1).

      ******************************************************
      *    REBATE FORMULA "A"
      *
      *    REBATE = LN-ANTICERN(1) * EADJ-FACTOR
      *                         + LN-TOTDEF
      ******************************************************
       EADJ-EARN-A SECTION.
           IF LN-ANTICERN(1) = 0
              MOVE 0 TO EADJ-FACTOR
           ELSE
              COMPUTE EADJ-FACTOR ROUNDED =
                 (EADJ-REBATE) / LN-ANTICERN(1).

           IF EADJ-FACTOR = 0
              MOVE EADJ-REBATE TO EADJ-ADJUST(1)
              MOVE "Y" TO EADJ-ADJUST-FG(1)
           ELSE
              MOVE 0 TO EADJ-SUB
              PERFORM EADJ-EARN-ADJUST 1 TIMES.

      ******************************************************
      *    (3)
      *    REBATE FORMULA FOR INTEREST (7) INCLUDES
      *    INTEREST + 1ST PAY EXT AND SERVICE CHARGES
      *
      *    REBATE = LN-ANTICERN(1) * EADJ-FACTOR
      *                         + LN-ANTICERN(3) * EADJ-FACTOR
      ******************************************************
       EADJ-EARN-3 SECTION.
           IF (LN-ANTICERN(1) + LN-ANTICERN(3)) = 0
              MOVE 0 TO EADJ-FACTOR
           ELSE
              COMPUTE EADJ-FACTOR ROUNDED =
                  EADJ-REBATE / (LN-ANTICERN(1) + LN-ANTICERN(3)).

           IF EADJ-FACTOR = 0
              MOVE EADJ-REBATE TO EADJ-ADJUST(1)
              MOVE "Y" TO EADJ-ADJUST-FG(1)
           ELSE
              MOVE 0 TO EADJ-SUB
              PERFORM EADJ-EARN-ADJUST 3 TIMES.

      *******************************************************
      *    ADJUSTMENT ROUTINE
      *******************************************************
       EADJ-EARN-ADJUST SECTION.
           ADD 1 TO EADJ-SUB.
           IF EADJ-SUB NOT = 2
              COMPUTE EADJ-ADJUST(EADJ-SUB) ROUNDED =
                             LN-ANTICERN(EADJ-SUB) * EADJ-FACTOR
              MOVE "Y" TO EADJ-ADJUST-FG(EADJ-SUB).
