      * COPYMEMBER: LIBWI/FEEENTLN
      *************************************************************************
      *            (FEEENTLN)
      *    VERSION FOR LN-FILE
      *      NOTE: IF YOU CHANGE THESE SECTIONS
      *            CHANGE ALSO 'FEEENTPD'
      * REV:
      *  JTG 090497 CHANGED FROM 'LIBWI/FEEENTLN1' --> 'LIBWI/FEEENTLN'
      *             CHANGED ALL FIELDS 'FW1-' --> 'FW-'
      *             ADDED USE OF FW-FEEFINCHG A90 WORLD
      *************************************************************************

      ***********************************************
      *    FEEENT SET WORKERS
      ***********************************************
       FEEENT-WIN-SET-WORKFEES SECTION.
           PERFORM FEEENT-WIN-SET-WORKFEES-DO
               VARYING SUB FROM 1 BY 1 UNTIL SUB > 10.

       FEEENT-WIN-SET-WORKFEES-EXIT.
           EXIT.

      * THIS IS PERFORM OTHER THAN FROM THIS COPY MEMBER:
       FEEENT-WIN-SET-WORKFEES-DO SECTION.
           MOVE LN-FEECODE(SUB) TO FW-FEECODE(SUB).
           MOVE LN-FEEAMT(SUB) TO FW-FEEAMT(SUB).
           MOVE LN-FEEOURS(SUB) TO FW-FEEOURS(SUB).
           MOVE LN-FEEFINCHG(SUB) TO FW-FEEFINCHG(SUB).

      ***********************************************
      *    FEEENT SET LN-FILE
      ***********************************************
       FEEENT-WIN-SET-LOANFEES SECTION.
           PERFORM FEEENT-WIN-SET-LOANFEES-DO
               VARYING SUB FROM 1 BY 1 UNTIL SUB >10.

       FEEENT-WIN-SET-LOANFEES-EXIT.
           EXIT.

      * THIS IS PERFORM OTHER THAN FROM THIS COPY MEMBER:
       FEEENT-WIN-SET-LOANFEES-DO SECTION.
           MOVE FW-FEECODE(SUB) TO LN-FEECODE(SUB).
           MOVE FW-FEEAMT(SUB) TO LN-FEEAMT(SUB).
           MOVE FW-FEEOURS(SUB) TO LN-FEEOURS(SUB).
           MOVE FW-FEEFINCHG(SUB) TO LN-FEEFINCHG(SUB).

