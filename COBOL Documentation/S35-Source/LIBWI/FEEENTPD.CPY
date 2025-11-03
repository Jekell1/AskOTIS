      * COPYMEMBER: LIBWI/FEEENTPD
      *************************************************************************
      *            (FEEENTPD)
      *    VERSION FOR PD-FILE
      *      NOTE: IF YOU CHANGE THESE SECTIONS
      *            CHANGE ALSO 'FEEENTLN'
      * REV:
      *  JTG 090497 CHANGED FROM 'LIBWI/FEEENTPD1' --> 'LIBWI/FEEENTPD'
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
           MOVE PD-FEECODE(SUB) TO FW-FEECODE(SUB).
           MOVE PD-FEEAMT(SUB) TO FW-FEEAMT(SUB).
           MOVE PD-FEEOURS(SUB) TO FW-FEEOURS(SUB).
           MOVE PD-FEEFINCHG(SUB) TO FW-FEEFINCHG(SUB).

      ***********************************************
      *    FEEENT SET PD-FILE
      ***********************************************
       FEEENT-WIN-SET-LOANFEES SECTION.
           PERFORM FEEENT-WIN-SET-LOANFEES-DO
               VARYING SUB FROM 1 BY 1 UNTIL SUB > 10.

       FEEENT-WIN-SET-LOANFEES-EXIT.
           EXIT.

      * THIS IS PERFORM OTHER THAN FROM THIS COPY MEMBER:
       FEEENT-WIN-SET-LOANFEES-DO SECTION.
           MOVE FW-FEECODE(SUB) TO PD-FEECODE(SUB).
           MOVE FW-FEEAMT(SUB) TO PD-FEEAMT(SUB).
           MOVE FW-FEEOURS(SUB) TO PD-FEEOURS(SUB).
           MOVE FW-FEEFINCHG(SUB) TO PD-FEEFINCHG(SUB).

