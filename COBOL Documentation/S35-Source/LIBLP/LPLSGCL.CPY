      * COPYMEMBER: LIBLP/LPLSCLG

      ***********************************************************************
      *    LPLSCL    LOAN STATUS INFORMATION CLEAR ROUTINE. GPS
      *
      * REV:
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ***********************************************************************
       CLEAR-LSG-FILE SECTION.
      * GPS TRACKING
           MOVE SPACES TO LSG-REC.
           MOVE 0 TO LSG-BRNO LSG-ACCTNO LSG-SEQNO LSG-LTOUCH-DATE.
           MOVE 0 TO LSG-LAST-COMMAND-DATE
                     LSG-LAST-AUTO-REPORT-DATE.
         
