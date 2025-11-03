      * COPYMEMBER: LIBLP/LPLXGCL
      **************************************************************************
      *    LPLXCL    LOAN PAYMENT TRAILER G/L CLEAR
      *
      * REV:
      *  04/09/2021 BAH NEW
      *  BAH 20210427 REMOVED FUTURE FIELDS
      **************************************************************************
       CLEAR-LXG-FILE SECTION.
           MOVE SPACES TO LXG-REC.

           MOVE 0 TO LXG-BRNO LXG-ACCTNO LXG-SEQNO.

           MOVE 0 TO LXG-LTOUCH-DATE.

           PERFORM VARYING LXG-SUB FROM 1 BY 1 UNTIL LXG-SUB > 3
              MOVE 0 TO LXG-GLNO(LXG-SUB)
                        LXG-GLAMT(LXG-SUB)
           END-PERFORM.
