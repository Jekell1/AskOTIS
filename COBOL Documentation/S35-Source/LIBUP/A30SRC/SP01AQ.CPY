      * COPYMEMBER: LIBSP/SP01AQ
      **************************************************
      *               (AQ)
      *    AUDITOR INQUIRY QUEUE
      *
      *    AQ-TYPE = "H" - HEADER RECORD
      *              " " - DETAIL RECORD
      *    AQ-SEQNO RUNS FROM 1 TO 999999:
      *    AQ-LOANDATE IS YYYYMMDD
      * 16/10/12 BAH CHANGED AQ-LOANDATE TO 9(8), INCREASED FILLER
      *              ADDED AQ-BRNO TO KEY
      **************************************************
       78  AQ-SIZE                     VALUE 75.
       01  AQ-REC.
           03  AQ2-KEY.
               05  AQ-TYPE              PIC X.
               05  AQ-SEQNO             PIC 9(6).
      * AQ-LOANDATE IS ALREADY SET AS YYYYMMDD
      *>>XDD DATE=YYYYMMDD
               05  AQ-LOANDATE          PIC 9(8).
               05  AQ1-KEY.
                   07  AQ-BRNO          PIC 9(4).
                   07  AQ-ACCTNO        PIC 9(8).
           03  AQ-RECORD.
               05  AQ-WORKED-FG         PIC X.
               05  FILLER               PIC X(51).
           03  AQ-HEADER-REC           REDEFINES AQ-RECORD.
               05  AQ-NEXT-SEQNO        PIC 9(6)    COMP-3.
               05  AQ-TOT-REC           PIC 9(6)    COMP-3.
               05  AQ-WORKED-REC        PIC 9(6)    COMP-3.
               05  FILLER               PIC X(36).
