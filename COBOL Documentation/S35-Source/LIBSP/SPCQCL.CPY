      * COPYMEMBER: LIBSP/SPCQCL
      ******************************************************************
      *  CLEAR  DAILY COLLECTOR QUEUE FILE
      *
      *  20190723 BAH NEW
      *  20210426  CS REMOVE FUTURE FIELDS
      *  20210527 BAH ADDED LOAD-CQ2-KEY
      ******************************************************************
       CLEAR-CQ-FILE SECTION.

           MOVE SPACES TO CQ-REC.

           MOVE 0 TO CQ-BRNO CQ-ACCTNO CQ-REV-BEHAVIOR-RISK
                     CQ-DATE CQ-START-TIME CQ-REV-DEL
                     CQ-REV-PDUE-DAYS CQ-REV-BAL CQ-SEQ
                     CQ-DATE-QUEUED.
