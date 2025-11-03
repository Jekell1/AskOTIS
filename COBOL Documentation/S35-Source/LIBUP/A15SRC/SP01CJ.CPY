      * COPYMEMBER: LIBSP/SP01CJ
      *****************************************************************
      *                         (CJ)
      *             COLLECTOR JOURNAL HISTORY FILE
      *
      *    IF CHANGES ARE MADE TO RECORD, ALSO CHANGE CLEAR ROUTINE.
      *
      * REV:
      *  MJD 990125 ----- UPDATED IN FLEXGEN -----
      *  BLV 122100 ADDED CJ-POSTING-BR - THIS WILL REFLECT CENTRAL
      *             POSTING BRANCH FOR GLOBAL QUEUING, OTHERWISE WILL
      *             BE 0, TFC PR# 60
      *  BLV 030207 ADDED CJ-QUEUE - THIS WILL RECORD THE QUEUE WHICH
      *             WAS SELECTED WHEN THE ACCOUNT WAS WORKED,    
      *             REGACC PR# 00023
      *****************************************************************
       01  CJ-REC.
           03  CJ1-KEY.
               05  CJ-BRNO              PIC 9(4)           COMP-3.
               05  CJ-COLLID            PIC XXX.
               05  CJ-DATE              PIC 9(5)           COMP-3.
               05  CJ-START-TIME        PIC 9(4)           COMP-3.
               05  CJ-SEQNO             PIC 99             COMP-3.
           03  CJ-RECORD.
               05  CJ-ACCTNO            PIC 9(8)           COMP-3.
               05  CJ-END-TIME          PIC 9(4)           COMP-3.
               05  CJ-ACTIVITY-CD       PIC X(3).
               05  CJ-RESULT-CD         PIC X(3).
               05  CJ-ACTION-CD         PIC X(4).
               05  CJ-NEW-COLLID        PIC X(3).
      *- - - - - - - - - - - - - - - - - - - - -  CONTRACTUAL STATUS - -
               05  CJ-CON-STAT          PIC X(3).
      *- - - - - - - - - - - - - - - - - - - - - - -  RECENCY STATUS - -
               05  CJ-REC-STAT          PIC X(3).
               05  CJ-FORM-NO           PIC 9(3).
               05  CJ-PROMISE-DATE1     PIC 9(6)           COMP-3.
               05  CJ-PROMISE-AMT       PIC 9(6)V99        COMP-3.
               05  CJ-BEG-LM-SEQ        PIC 9(2).
               05  CJ-END-LM-SEQ        PIC 9(2).
               05  CJ-PROMISE-DATE2     PIC 9(6)           COMP-3.
               05  CJ-CUR-BAL           PIC 9(6)V99        COMP-3.
               05  CJ-PAST-DUE          PIC 9(6)V99        COMP-3.
               05  CJ-POSTING-BR        PIC 9(4)           COMP-6.
               05  CJ-QUEUE             PIC X(3).
               05  CJ-FILLER            PIC X(6).
