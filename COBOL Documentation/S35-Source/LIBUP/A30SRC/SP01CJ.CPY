      * COPYMEMBER: LIBSP/SP01CJ
      *****************************************************************
      *                         (CJ)
      *             COLLECTOR JOURNAL HISTORY FILE
      *
      *    IF CHANGES ARE MADE TO RECORD, ALSO CHANGE CLEAR ROUTINE.
      *         CJ2-KEY = CJ-BRNO CJ-ACCTNO CJ-DATE CJ-START-TIME
      *                      WITH DUPLICATES
      *
      * REV:
      *  MJD 990125 ----- UPDATED IN FLEXGEN -----
      *  BLV 122100 ADDED CJ-POSTING-BR - THIS WILL REFLECT CENTRAL
      *             POSTING BRANCH FOR GLOBAL QUEUING, OTHERWISE WILL
      *             BE 0, TFC PR# 60
      *  BLV 030207 ADDED CJ-QUEUE - THIS WILL RECORD THE QUEUE WHICH
      *             WAS SELECTED WHEN THE ACCOUNT WAS WORKED,
      *             REGACC PR# 00023
      *  BAH 160601 A30 EXPANSION CHANGES CJ-DATE IS ALREADY USING ALL 8
      *  BAH 180125 CHANGED SIZE FROM 165 TO 125, REDUCED FILLER
      *****************************************************************
       78  CJ-SIZE                      VALUE 125.
       01  CJ-REC.
           03  CJ1-KEY.
               05  CJ-BRNO              PIC 9(4).
               05  CJ-COLLID            PIC XXX.
      *>>XDD DATE=YYYYMMDD * ALREADY SET TO ALL 8 DIGITS!
               05  CJ-DATE              PIC 9(8).
               05  CJ-START-TIME        PIC 9(4).
               05  CJ-SEQNO             PIC 99.

      *>>XDD DATE=YYYYMMDD 
           03  CJ-LTOUCH-DATE           PIC 9(8)           COMP-3.
           03  CJ-ACCTNO                PIC 9(8).
           03  CJ-END-TIME              PIC 9(4)           COMP-3.
           03  CJ-ACTIVITY-CD           PIC X(3).
           03  CJ-RESULT-CD             PIC X(3).
           03  CJ-ACTION-CD             PIC X(4).
           03  CJ-NEW-COLLID            PIC X(3).
      *- - - - - - - - - - - - - - - - - - - - -  CONTRACTUAL STATUS - -
           03  CJ-CON-STAT              PIC X(3).
      *- - - - - - - - - - - - - - - - - - - - - - -  RECENCY STATUS - -
           03  CJ-REC-STAT              PIC X(3).
           03  CJ-FORM-NO               PIC 9(4)           COMP-3.
      *>>XDD DATE=YYYYMMDD 
           03  CJ-PROMISE-DATE1         PIC 9(8)           COMP-3.
           03  CJ-PROMISE-AMT           PIC 9(7)V99        COMP-3.
           03  CJ-BEG-LM-SEQ            PIC 9(3)           COMP-3.
           03  CJ-END-LM-SEQ            PIC 9(3)           COMP-3.
      *>>XDD DATE=YYYYMMDD 
           03  CJ-PROMISE-DATE2         PIC 9(8)           COMP-3.
           03  CJ-CUR-BAL               PIC 9(7)V99        COMP-3.
           03  CJ-PAST-DUE              PIC 9(7)V99        COMP-3.
           03  CJ-POSTING-BR            PIC 9(4)           COMP-3.
           03  CJ-QUEUE                 PIC X(3).

           03  FILLER                   PIC X(31).
