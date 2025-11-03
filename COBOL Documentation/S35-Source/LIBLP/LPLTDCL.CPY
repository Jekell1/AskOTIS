      * COPYMEMBER: LIBLP/LPLTDCL
      **************************************************************************
      *    LPLTDCL    DEBIT CARD TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  02/21/2018 BAH NEW, WORLD #1306 NETSPEND
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      **************************************************************************
       CLEAR-LTD-FILE SECTION.
           MOVE SPACES TO LTD-REC.

           MOVE 0 TO LTD-LTOUCH-DATE.

           MOVE 0 TO LTD-DEBIT-CARD-NUMBER
                     LTD-DEBIT-CARD-AMT LTD-VOID-DATE
                     LTD-CLEARED-DATE LTD-CLEARED-AMT
                     LTD-LOANDATE.


