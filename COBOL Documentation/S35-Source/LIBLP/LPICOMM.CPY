      * COPYMEMBER: LIBLP/LPICOMM

      *************************************************************
      *          INSURANCE COMMISSION CALCULATION
      *
      *   NAME:  LPICOMM
      *   DESC:  THIS ROUTINE COMPUTES THE INSURANCE COMMISSION
      *          FOR A GIVEN INSURANCE LEVEL OF THE LOAN RECORD
      *
      *          NOTE:
      *               THIS ROUTINE REQUIRES THAT THE INSURANCE
      *               FILE AND IO ROUTINES ARE THE IN PROGRAM
      *
      *   IN  :  ICOMM            - PD-INSURANCE LEVEL (1 TO 8)
      *          ICOMM-INSCOMP    - INSURANCE COMPANY
      *          ICOMM-INSOURS    - INSURANCE OURS
      *          ICOMM-INSPREM    - INSURANCE PREMIUM
      *
      *   OUT :  ICOMM-INSCOMM    - INSURANCE COMMISSION
      *   COPY:  LPICOMMW
      *
      * REV:
      *  JTG 062095 ADDED LOGIC FOR 10 INSURANCE LEVELS
      *  JTG 111396 CHANGED LOGIC FOR 8 INSURANCE LEVELS
      *  JTG 012797 CORRECTED FOR PP2 COMM & O3 COMM
      *  MJD 991001 ADDED LOGIC TO COMPUTE COMMISSION AS EITHER % OR
      *             FIXED AMOUNT  (TFC PR#41)
      *   CS 050426 DONT COMPUTE COMMISSION IF PREMIUM IS ZERO [PR#479 NSA]
      *  BAH 20200615 REMOVED OPEN-IC1-FILE AND CLOSE-IC1-FILE #TST001
      ******************************************************************
       ICOMM-CALCULATION SECTION.
           MOVE 0 TO ICOMM-INSCOMM.
           IF ICOMM = 0
                 OR ICOMM > 8
                   OR PD-INSCOMP(ICOMM) = 0
                     OR PD-INSPREM(ICOMM) = ZERO
              GO TO ICOMM-CALCULATION-EXIT.

      * CL DECREASING
           IF PD-INSURED = "J"
              MOVE 2 TO ICOMM-SUB1
           ELSE
              MOVE 1 TO ICOMM-SUB1.
      * CL LEVEL
           IF PD-INSTYPES-LL
              ADD 2 TO ICOMM-SUB1.

      * A&H
           IF ICOMM = 2
              MOVE 5 TO ICOMM-SUB1
           ELSE
      * PP
           IF ICOMM = 3
              IF PD-INSTYPES-PP2
                 MOVE 7 TO ICOMM-SUB1
              ELSE
                 MOVE 6 TO ICOMM-SUB1
              END-IF
           ELSE
      * O1
           IF ICOMM = 4
              MOVE 8 TO ICOMM-SUB1
           ELSE
      * O2 TO O5
           IF ICOMM > 4
              ADD 4 ICOMM GIVING ICOMM-SUB1.

           IF PD-INSCOMP(ICOMM) NOT = ICOMM-IC-CODE
              MOVE PD-INSCOMP(ICOMM) TO IC-CODE
              PERFORM READ-IC1-FILE
              IF IO-FG NOT = 0
                 MOVE 0 TO ICOMM-IC-CODE
                 MOVE 100 TO IC-COMMRATE(ICOMM-SUB1)
              ELSE
                 MOVE IC-CODE TO ICOMM-IC-CODE.
           IF PD-INSOURS(ICOMM) = "Y"
              IF IC-COMMRATE(ICOMM-SUB1) >= 0
                 COMPUTE ICOMM-INSCOMM ROUNDED =
                           IC-COMMRATE(ICOMM-SUB1)
                                    / 100 * PD-INSPREM(ICOMM)
              ELSE
                   COMPUTE ICOMM-INSCOMM =
                             IC-COMMRATE(ICOMM-SUB1) * -1
              END-IF.


       ICOMM-CALCULATION-EXIT.
           EXIT.
