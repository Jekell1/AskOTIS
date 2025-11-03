      * COPYMEMBER: LIBSP/SPFDAQ
      **************************************************
      *               (AQ)
      *    AUDITOR INQUIRY QUEUE
      *
      *    AQ-TYPE = "H" - HEADER RECORD
      *              " " - DETAIL RECORD
      *    AQ-SEQNO RUNS FROM 1 TO 999999:
      *    AQ-LOANDATE IS JULIAN:
      **************************************************
       FD  OAQ-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/SP01AQ.CPY" REPLACING LEADING "AQ" BY "OAQ".
