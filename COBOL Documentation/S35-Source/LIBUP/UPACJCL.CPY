      * COPYMEMBER: LIBUP/UPCJCL

      ***********************************************************************
      *      ASCII COLLECTOR JOURNAL FILE CLEAR ROUTINE
      *
      ***********************************************************************
       CLEAR-ACJ-FILE SECTION.
           MOVE ALL X"09" TO ACJ-REC.

           MOVE 0 TO ACJ-BRNO ACJ-DATE ACJ-START-TIME ACJ-SEQNO.

           MOVE 0 TO ACJ-ACCTNO ACJ-END-TIME ACJ-FORM-NO
                     ACJ-PROMISE-DATE1 ACJ-PROMISE-AMT ACJ-BEG-LM-SEQ
                     ACJ-END-LM-SEQ ACJ-PROMISE-DATE2 ACJ-CUR-BAL
                     ACJ-PAST-DUE ACJ-POSTING-BR.

           MOVE SPACES TO ACJ-COLLID ACJ-ACTIVITY-CD ACJ-RESULT-CD     
                          ACJ-ACTION-CD ACJ-NEW-COLLID ACJ-CON-STAT      
                          ACJ-REC-STAT ACJ-QUEUE.

