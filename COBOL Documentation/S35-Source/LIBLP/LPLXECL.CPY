      * COPYMEMBER: LIBLP/LPLXECL
      **************************************************************************
      *    LPLXECL    LOAN PAYMENT TRAILER EARNINGS CLEAR
      *
      * REV:
      *  04/09/2021 BAH NEW
      **************************************************************************
       CLEAR-LXE-FILE SECTION.
           MOVE SPACES TO LXE-REC.

           MOVE 0 TO LXE-BRNO LXE-ACCTNO LXE-SEQNO.

           MOVE 0 TO LXE-LTOUCH-DATE
                     LXE-WORKER
                     LXE-WORKER2.

           PERFORM VARYING LXE-SUB FROM 1 BY 1 UNTIL LXE-SUB > 7
              MOVE 0 TO LXE-EARN(LXE-SUB)
           END-PERFORM.
