      * COPYMEMBER: LIBLP/LPDTCL

      ***********************************************************************
      *    LPDTCL    DEALER TRANSACTION FILE CLEAR ROUTINE
      *
      * REV:
      *  JTG 072696 ADDED CLEAR OF DT-BATCHID-SEQ
      *  CS  060320 ADDED CLEAR OF DT-BIZ-EVENT-CD.
      *  CS  060707 REMOVED CLEAR OF DT-EVENT-CD, ITS ALPHA
      *  CS  190501 CLEAR FUTURE FIELDS
      *  AMM 0427214 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-DT-FILE SECTION.
           MOVE SPACES TO DT-REC.

           MOVE 0 TO DT-DLNO DT-CLASS DT-ACCTNO
                     DT-DATE DT-SEQ.
           MOVE 0 TO DT-BRNO DT-LPSEQNO
                     DT-DLRVPRIN(1) DT-DLRVPRIN(2) DT-DLRVPRIN(3)
                     DT-DLRVCHGS(1) DT-DLRVCHGS(2) DT-DLRVCHGS(3)
                     DT-DLPARVPAID DT-DLPARVHELD DT-PROCEEDS
                     DT-RELEASED DT-PAYMENTS DT-CKNO DT-CKDATE
                     DT-BATCHID DT-BATCHID-SEQ DT-PURBAL DT-APCUR
                     DT-DLRVNO
                     DT-DLRVPRIN-PAY DT-DLRVCHGS-PAY DT-DLPARVHELD-PAY.

