      * COPYMEMBER: LIBSP/SPCJCL

      ***********************************************************************
      *    LPCJCL    COLLECTOR JOURNAL FILE CLEAR ROUTINE
      *
      * REV:
      *  101001 BAH ADDED REST OF THE NUMERIC FIELDS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      ***********************************************************************
       CLEAR-CJ-FILE SECTION.
           MOVE SPACES TO CJ-REC.
           MOVE 0 TO CJ-BRNO CJ-DATE CJ-START-TIME CJ-SEQNO.

           MOVE 0 TO CJ-ACCTNO CJ-END-TIME CJ-FORM-NO
                     CJ-PROMISE-DATE1 CJ-PROMISE-AMT CJ-BEG-LM-SEQ
                     CJ-END-LM-SEQ CJ-PROMISE-DATE2 CJ-CUR-BAL
                     CJ-PAST-DUE CJ-POSTING-BR.

