      ******************************************************************
      * REV:
      *  20190723 BAH NEW
      *  20200610 BAH ADDED AQS-LTOUCH-DATE
      *  20210426 AMM REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-AQS-FILE SECTION.

           MOVE SPACES TO AQS-REC.

           MOVE 0 TO AQS-BRNO AQS-NEXT-SEQNO AQS-TOT-REC
                     AQS-WORKED-REC AQS-LTOUCH-DATE.

