      * COPYMEMBER: LIBSP/SP01AQ
      ******************************************************************
      *
      * AUDITOR INQUIRY QUEUE
      *
      *=================================================================
      *
      * DETAIL RECORD:
      *    AQ-BRNO   = NON-ZERO (HAS BRANCH NUMBER IN IT)
      *
      *=================================================================
      *
      * AQ1-KEY = AQ-BRNO, AQ-ACCTNO
      * AQ2-KEY = AQ-BRNO, AQ-SEQNO, AQ-LOANDATE, AQ-ACCTNO
      *
      * AQ-SEQNO RUNS FROM 1 TO 999999
      * 
      *=================================================================
      * 2019-0130 KEC. IN A15 & A30, THIS FILE WAS LOCAL.  THE FILE WAS
      * ONLY CREATED BY SP/AQEXTR (AS WELL AS MN/AQMAIN).  IT IS ONLY
      * USED IN LP/AUDIQ0 AND WI/AQSCAN.
      * WITHIN SP/AQEXTR, IT DID AN OPEN-OUTPUT ON THE AQFILE WHICH THEN
      * CREATES AN AQFILE AND AQ RECORDS FOR ONLY THE AUDITOR'S LOGIN
      * BRANCH (EXT-CONAME-USER-LOC).
      * FOR A32 & A34, WITHIN SP/AQEXTR, GOING TO ONLY PURGE THE AQ-REC
      * FOR THE AUDITOR'S LOGIN BRANCH.  FOR LP/AUDIQ0, GOING TO USE
      * AQ2-KEY FOR READ-NEXT LOOKING FOR ONLY THE AUDITOR'S LOGIN
      * BRANCH.  THE AQ2-KEY HAS TO HAVE THE AQ-BRNO IN IT FOR THIS
      * TO WORK.
      * 
      *=================================================================
      * REV:
      * 16/10/12 BAH CHANGED AQ-LOANDATE TO 9(8), INCREASED FILLER
      *              ADDED AQ-BRNO TO KEY
      * 012319 AMM LOCAL TO GLOBAL AQ FILE
      * 032619 AMM CREATED NEW AQS FILE FOR HEADER/SUMMARY FIELDS
      * 190515 CS  ADD FUTURE FIELDS
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20200610 ADDED AQ-LTOUCH-DATE
      * AMM 20210423 REMOVED FUTURE FIELDS, RESIZED FILE
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  AQ-SIZE                     VALUE 70.
       01  AQ-REC.
           03  AQ1-KEY.
               05  AQ-BRNO             PIC 9(4).
               05  AQ-ACCTNO           PIC 9(8).
      $XFD DATE=YYYYMMDD
           03  AQ-LOANDATE             PIC 9(8).
           03  AQ-SEQNO                PIC 9(6).
           03  AQ-WORKED-FG            PIC X(1).

      $XFD DATE=YYYYMMDD
           03  AQ-LTOUCH-DATE          PIC 9(8).
           03  FILLER                  PIC X(35).
