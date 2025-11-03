      * COPYMEMBER: LIBLP/LPCACL
      ******************************************************************
      *    LPCACL    CASH DRAWER FILE CLEAR ROUTINE
      *
      * REV:
      *  062498 ADDED CA-TILLNO-ZERO, REMOVED CA-TILLNO, ITS SPACES
      *  BLV 980923 ADDED CA-NET-CASH-NOTES
      *  BLM 160920 ADDED CA-CHECK-DEPOSIT, WORLD PR# 1188
      *   CS 210423 REMOVE FUTURE FIELDS
      ******************************************************************
       CLEAR-CA-FILE SECTION.
           MOVE SPACES TO CA-REC.
           MOVE 0      TO CA-BRNO CA-DATE CA-TILLNO CA-BEGBAL
                          CA-OPENED-HHMM CA-CLOSED-HHMM  CA-ENDBAL 
                          CA-RUNNING CA-DEPOSIT CA-NET-CASH-NOTES
                          CA-CHECK-DEPOSIT.


