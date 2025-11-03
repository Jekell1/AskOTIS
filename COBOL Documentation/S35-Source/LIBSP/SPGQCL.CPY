      ******************************************************************
      * REV:
      *  20190723 BAH NEW
      *   20210506 CS REMOVE FUTURE FIELDS
      ******************************************************************
       CLEAR-GQ-FILE SECTION.

           MOVE SPACES TO GQ-REC.

           MOVE 0 TO GQ-BRNO GQ-ACCTNO GQ-DATE GQ-START-TIME
                     GQ-REV-DEL GQ-REV-PDUE-DAYS GQ-REV-BAL
                     GQ-SEQ GQ-LTOUCH-DATE GQ-DATE-QUEUED.
 
