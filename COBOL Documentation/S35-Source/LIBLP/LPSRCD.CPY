      * COPYMEMBER: LIBLP/LPSRCD
      ******************************
      *    GET BY-TYPE SUBSCRIPT
      ******************************

      **************************************************************************
      *         GET SUBSCRIPT FOR SOURCE CODE TABLES
      *
      *   NAME: LPSRCD
      *   DESC: THIS ROUTINE DETERMINES THE TABLE INDEX
      *         FOR REPORTS AND INQUIRES
      *
      *   IN  :
      *   OUT : SRCD-FG:
      *            SRCD-IS-PL  - "Y" - UPDATE PL-TABLE
      *            SRCD-NOT-PL - "N" - UPDATE PERIOD-TABLE
      *
      *        IF SRCD-NOT-PL:
      *                        SRCD = PERIOD TABLE INDEX
      *                           1 - NB            7 - RN
      *                           2 - SEE BELOW     8 - SC OR DM
      *                           3 - FB            9 - OT
      *                           4 - PB           10 - RB
      *                           5 - PBD          11 - RO
      *                           6 - PBI
      *
      *                          IF GP-THE-ONLY-SF-SRCD NOT = BLANK
      *                             IF IS EQUAL TO LN-SRCD
      *                                SRCD = 2      SALES FINANCE
      *                             ELSE
      *                                SRCD = 9 (RF) DIRECT LOAN CLASS
      *
      *        IF SRCD-IS-PL:
      *                        SRCD = PL TABLE INDEX
      *                          1 - PL ACTIVE     2 - PL INACTIVE
      *                          3 - P2 ACTIVE     4 - P2 INACTIVE
      *                          5 - P3 ACTIVE     6 - P3 INACTIVE
      *                          7 - P4 ACTIVE     8 - P4 INACTIVE
      *                          9 - P5 ACTIVE    10 - P5 INACTIVE
      *
      *REV:
      * MJD 081195 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      * JTG 082495 CORRECTED  LOGIC FOR GP-THE-ONLY-SF-SRCD
      * BLL 012496 NEW LOGIC FOR SEPARATE PL TABLE
      * CS  042798 ADDED NEW SOURCE CODE RENEWALS "RB" AND "RO"
      * JTG 030318 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      **************************************************************************
       GET-SRCD-SUB SECTION.

           MOVE 0 TO SRCD.
           IF LN-PLCD = " "
              MOVE "N" TO SRCD-FG
              PERFORM GET-SRCD-SUB-NO-PL
              GO TO EXIT-GET-SRCD.

           MOVE 1 TO SRCD.
           IF LN-PLTYPE = "2" MOVE 3 TO SRCD
              ELSE IF LN-PLTYPE = "3" MOVE 5 TO SRCD
              ELSE IF LN-PLTYPE = "4" MOVE 7 TO SRCD
              ELSE IF LN-PLTYPE = "5" MOVE 9 TO SRCD.
           IF LN-PLCD NOT = "P"
              ADD 1 TO SRCD.
           MOVE "Y" TO SRCD-FG.

       EXIT-GET-SRCD.
           EXIT.

      ***********************************************
       GET-SRCD-SUB-NO-PL SECTION.
      ***********************************************

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                 MOVE 2 TO SRCD
                 GO TO GET-SRCD-SUB-EXIT
              ELSE
                 IF LN-SRCD = "RF"
                    MOVE 9 TO SRCD
                    GO TO GET-SRCD-SUB-EXIT.

           IF LN-SRCD = "NB"
              MOVE 1 TO SRCD
           ELSE
           IF LN-SRCD = "RF"
              MOVE 2 TO SRCD
           ELSE
           IF LN-SRCD = "FB"
              MOVE 3 TO SRCD
           ELSE
           IF LN-SRCD = "PB"
              PERFORM GET-SRCD-PB
           ELSE
           IF LN-SRCD = "RN"
              MOVE 7 TO SRCD
           ELSE
           IF LN-SRCD = "DM" AND GP-NO-SC-USE-DM = "Y"
              MOVE 8 TO SRCD
           ELSE
           IF LN-SRCD = "SC" AND GP-NO-SC-USE-DM NOT = "Y"
              MOVE 8 TO SRCD
           ELSE
           IF LN-SRCD = "RB"
              MOVE 10 TO SRCD
           ELSE
           IF LN-SRCD = "RO"
              MOVE 11 TO SRCD
           ELSE
              MOVE 9 TO SRCD.
       GET-SRCD-SUB-EXIT.
           EXIT.

      *********************************************************
      *    GET SRCD INDEX FOR PB
      *    DETERMINE IF ACCOUNT IS 'PB' 'PBD' OR 'PBI'
      *
      * NOTE: ACCOUNTS WILL BE CATAGORIZED AS
      *       'PB DELINQ' IF BOTH CATAGORIES APPLY.
      * 9/25/97 - FOR WORLD: IF SP-PBINCR-PERCENT = 1, JUST
      *           CHECK IF LNAMT IS > PRIOR FOR SRCD OF 6
      *********************************************************
       GET-SRCD-PB SECTION.
           MOVE 4 TO SRCD.
           IF SP-PBDEL-DAYS NOT = 0
              IF LN-PRPDUE-DAYS > SP-PBDEL-DAYS
      * PBD
                 MOVE 5 TO SRCD
                 GO TO GET-SRCD-PB-EXIT.
           IF SP-PBINCR-PERCENT NOT = 0
              IF SP-PBINCR-PERCENT = 1
                 IF LN-LNAMT > LN-PRLNAMT
      * PBI
                    MOVE 6 TO SRCD
                 END-IF
              ELSE
                 COMPUTE SRCD-WORKER ROUNDED =
                     LN-PRLNAMT - 0.01
                         + LN-PRLNAMT * SP-PBINCR-PERCENT / 100
                 IF LN-LNAMT > SRCD-WORKER
                    MOVE 6 TO SRCD.

       GET-SRCD-PB-EXIT.
           EXIT.
