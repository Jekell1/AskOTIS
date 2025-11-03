      * COPYMEMBER: LIBGB/GBGPCL

      ******************************************************************
      *    GBGPCL    GLOBAL PARAMETERS CLEAR ROUTINE GPFILE
      *
      * REV:
      *  20190723 BAH NEW
      *  20200114 BAH REMOVED GP-AP FIELDS
      ******************************************************************
       CLEAR-GP-FILE SECTION.
           MOVE SPACES TO GP-REC.

           MOVE 0 TO GP-BEG-FISCAL-DATE   GP-BANK-GL
                     GP-BANK-CLEARING-GL  GP-BANK-ROUTING-NO
                     GP-BANK-ACCTNO       GP-TR-RETAIN-MONTHS
                     GP-FED-TAX-ID
                     GP-BANK-DEPOSIT-GL   GP-EOD-RETAIN-MONTHS
                     GP-DEFAULT-O3-INS-COMPANY
                     GP-DEFAULT-ACCRATE   GP-NEXT-NOTE
                     GP-NEXT-BATCH-ID     GP-TIME-ZONE-FACTOR
                     GP-CALL-START-TIME   GP-CORP-BRNO
                     GP-PROM-AMT-MIN-PCT  GP-EARLIEST-CLOSE-TIME
                     GP-CHBORR-PAST-DUE-DAYS
                     GP-BW-SCORES-QUARTER-SUB
                     GP-NEXT-EMPLOYEE-NO GP-MAX-PROMISE-DAYS
                     GP-DAYS-IDENT-NEW-PAPER
                     GP-EARLIEST-OPEN-TIME
                     GP-BATCH-REFCD-DUP-DAYS.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              MOVE 0 TO GP-END-FISCAL-DATE(SUB)
              IF SUB NOT > 3
                 MOVE 0 TO GP-PROGRP-LN-CLASS(SUB)
              END-IF
              IF SUB NOT > 4
                 MOVE 0 TO GP-PROGRP-SF-CLASS(SUB)
              END-IF
           END-PERFORM.

           MOVE 100       TO GP-BP-NEXT-BATCH.
           MOVE 999999.99 TO GP-AMT-CASH-PROCEEDS.
           MOVE 200       TO GP-LB-NEXT-BATCH.
           MOVE  24       TO GP-CALL-END-TIME.
           MOVE 500       TO GP-TRUE-NEXT-BATCH.
           MOVE 600       TO GP-MGRAM-NEXT-BATCH.
           MOVE 700       TO GP-REFCD-NEXT-BATCH.
           MOVE 900       TO GP-OTREF-NEXT-BATCH.


