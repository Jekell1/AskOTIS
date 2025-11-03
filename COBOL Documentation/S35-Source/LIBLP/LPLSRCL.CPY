      * COPYMEMBER: LIBLP/LPLSCLR

      ***********************************************************************
      *    LPLSCL    LOAN STATUS INFORMATION CLEAR ROUTINE. RESCHEDULE
      *
      * REV:
      *  BAH 110328 ADDED CLEAR OF "R" RECORD REGACC PR# 289/290
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190506 CLEAR FUTURE FIELDS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      ***********************************************************************
       CLEAR-LSR-FILE SECTION.
      * RESCHEDULE/SCRA
           MOVE SPACES TO LSR-REC.
           MOVE 0 TO LSR-BRNO LSR-ACCTNO LSR-SEQNO LSR-LTOUCH-DATE.
           MOVE 0 TO LSR-SCRA-START-DATE LSR-SCRA-END-DATE
                     LSR-CURRENT-RESCHED-DATE
                     LSR-INITIAL-RESCHED-DATE
                     LSR-INITIAL-TDR-DATE LSR-CURRENT-TDR-DATE.

