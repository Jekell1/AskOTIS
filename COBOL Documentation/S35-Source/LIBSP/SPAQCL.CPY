      ******************************************************************
      * REV:
      *  20190723 BAH NEW
      *  20200610 BAH ADDED AQ-LTOUCH-DATE
      *  20210423 AMM REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-AQ-FILE SECTION.

           MOVE SPACES TO AQ-REC.

           MOVE 0 TO AQ-BRNO AQ-ACCTNO AQ-SEQNO
                     AQ-LOANDATE AQ-LTOUCH-DATE.

