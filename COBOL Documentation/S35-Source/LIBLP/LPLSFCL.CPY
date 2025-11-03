      * COPYMEMBER: LIBLP/LPLSFCL 

      ***********************************************************************
      *    LPLSCL    LOAN STATUS INFORMATION CLEAR ROUTINE. FOLLOWUP
      *
      * REV:
      *  BAH 070308 ADDED CLEAR OF "F" RECORD FOLLOW UP INFO TRITON PR# 2937
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190503 CLEAR FUTURE FIELDS
      * AMM 20210429 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-LSF-FILE SECTION.
           MOVE SPACES TO LSF-REC.
           MOVE 0 TO LSF-BRNO LSF-ACCTNO LSF-SEQNO
                     LSF-LTOUCH-DATE.
           MOVE 0 TO LSF-SKIP-START-DATE LSF-SKIP-END-DATE
                     LSF-OFR-START-DATE  LSF-OFR-END-DATE
                     LSF-IMPOUND-START-DATE  LSF-IMPOUND-END-DATE
                     LSF-BNKRPT-START-DATE  LSF-BNKRPT-END-DATE
                     LSF-INSUR-START-DATE  LSF-INSUR-END-DATE
                     LSF-COLLECT-START-DATE  LSF-COLLECT-END-DATE.

