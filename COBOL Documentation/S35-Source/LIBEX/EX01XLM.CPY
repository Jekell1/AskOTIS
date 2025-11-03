      * COPYMEMBER: LIBEX/EX01XLM
      ********************************************************************
      * LOAN MEMO EXTRACTION FILE       93  BYTES
      *
      *  DATA WAREHOUSE EXTRACTION
      *
      ********************************************************************
      * REV:
      *  041204 BAH NEW FOR CENTRIX, PR# 2416
      ********************************************************************
       78  XLM-SIZE                VALUE 93.

       01  XLM-REC.
           03  XLM-BRACCTNO        PIC 9(10).
           03  XLM-ACCTNO          PIC 9(8).
           03  XLM-DATE            PIC 9(8).
           03  XLM-SEQ             PIC 99.
           03  XLM-MEMO            PIC X(60).
           03  XLM-MEMO-TYPE       PIC X.
           03  XLM-REGION-NUMBER   PIC 9(2).
           03  XLM-CR              PIC X(1).
           03  XLM-LF              PIC X(1).
