      * COPYMEMBER: LIBLP/LPLSMCL

      ***********************************************************************
      *    LPLSMCL    LOAN MISC FIELDS CLEAR ROUTINE
      *
      * REV:
      *  BAH 20220921 NEW #1570
      ***********************************************************************
       CLEAR-LSM-FILE SECTION.
      * DEFICIENCY RECORD
           MOVE SPACES TO LSM-REC.

           MOVE 0 TO LSM-BRNO LSM-ACCTNO LSM-SEQNO LSM-LTOUCH-DATE.
           MOVE 0 TO LSM-TRANS-FROM-BRNO
                     LSM-TRANS-FROM-ACCTNO
                     LSM-TRANS-FROM-DATE
                     LSM-MISPAY-90-DATE
                     LSM-SECOND-BEN-PHONE.
