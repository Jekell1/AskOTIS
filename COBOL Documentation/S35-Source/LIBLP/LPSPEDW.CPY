      * COPYMEMBER: LIBLP/LPSPEDW
      **************************************************************************
      *                LPSPEDW
      *   SP - EDIT TABLE VERIFICATION WORKERS
      *
      * REV:
      *  JTG 102089 ADDED SPDUAL-LIAB FOR (GA)
      *  JTG 050692 ADDED VERIFY-AGE, VERIFY-REFDATE
      *                   VERIFY-TODAY-DATE
      *  BAH 110995 ADDED BAGE-TEST FOR TESTING AGE RESTRICTIONS
      *  JTG 000113 ADDED VERIFY-REJECT-TERM-FG, VERIFY-REJECT-TERM(8)
      *             RE: SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *             NO OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      *  JTG 000127 CHANGED VERIFY-REJECT-TERM(8) FROM 999 TO S9999
      *  BAH 070226 ADDED VERIFY-MAKER-SUB TO KNOW WHICH MAKER YOU ARE
      *             TESTING WHEN IN A LOOP, SUPPORT PL# 580
      *  BAH 121009 ADDED WORK-HOURS WORKER FOR MULLEN PR# 4088
      *  BAH 161025 INCREASED DATES AND VERIFY-AGE TO 9(8)
      *  BAH 20230301 ADDED WORKERS FOR SPTEST-INSCOVR-LIMITS #1594
      *  BHA 20240412 REMOVED OUTSIDE-LIMITS
      **************************************************************************
       01  VERIFY-TERMCEIL-INPUT.
           03  VERIFY-TODAY-DATE          PIC 9(8).
           03  VERIFY-TERMCEIL-APR        PIC 999V999.
           03  VERIFY-TERMCEIL-EFF-FG     PIC X.
           03  VERIFY-TERMCEIL-EFF        PIC 999V999.
           03  VERIFY-AGE                 PIC 9(8)     VALUE 0.
           03  VERIFY-REFDATE             PIC 9(8).
           03  SPTEST-CD-SECURED          PIC XX.
           03  VERIFY-REJECT-TERM-FG      PIC X        VALUE " ".
           03  VERIFY-MAKER-SUB           PIC 9        VALUE 1.
       01  VERIFY-REJECT-TERM-WORKERS.
           03  VERIFY-REJECT-TERM         OCCURS 8
                                          PIC S9(4).
       01  VERIFY-TERMCEIL-WORKERS.
           03  VALID-LOAN                 PIC X.
           03  LIMIT-ERROR                PIC X.
           03  N-WK1                      PIC S9(8)V99.
           03  LIMIT-AMT-TYPE             PIC X(25).
           03  LIMIT-TYPE-LIT             PIC X(25).
           03  LIMIT-COLL-TYPE            PIC X(20).
           03  LIMIT-WORKER               PIC X.
           03  LIMIT-AMT                  PIC $(8).99.
           03  LIMIT-TERM                REDEFINES LIMIT-AMT
                                          PIC Z(10)9.
           03  LIMIT-RATE                REDEFINES LIMIT-AMT
                                          PIC Z(6).9(4).
           03  LIMIT-AMT-LIT             REDEFINES LIMIT-AMT
                                          PIC X(11).
       01  VERIFY-STEP-TERM-WORKERS.
           03  ST-MINAMT                  PIC S9(8)V99.
           03  ST-MAXAMT                  PIC S9(8)V99.
           03  ST-MINTERM                 PIC 999.
           03  ST-MAXTERM                 PIC 999.
           03  STSUB-1                    PIC 9.
           03  STSUB-2                    PIC 9.
           03  ST-FIRST-STEP              PIC X.

      * SPTEST-INSCOVR-LIMITS WORKERS
       01  SPTEST-MAX-INS-DISPLAY         PIC 9(6).99.
       01  SPTEST-MAX-INS-CHARGEABLE      PIC 9(6)V99.
       01  SPTEST-INSCOVR-TEST            PIC 9(6)V99.

       01  VERIFY-SPTEST-WORKERS.
           03  SPTEST-INSTYPE             PIC XXX.
           03  SPTEST-MFYEARLY            PIC 9(4)V99.
           03  SPTEST-BENEFIT             PIC 9(6)V99.
           03  SPT1                       PIC 9.
           03  SPT2                       PIC 9.

       01  SPDUAL-LIAB                    PIC S9(7)V99  VALUE 0.
       01  BAGE-TEST                      PIC 9(3) VALUE 0.
       01  WORK-HOURS-ALPHA               PIC X(9).
       01  WORK-HOURS-NUM REDEFINES WORK-HOURS-ALPHA PIC 9(9).
