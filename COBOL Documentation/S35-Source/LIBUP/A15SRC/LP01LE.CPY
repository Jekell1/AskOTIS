      * COPYMEMBER: LIBLP/LP01LE
      ******************************************************************
      *   LOAN ENDORSER FILE        (LE)        BYTES = 48           
      *       KEY - ACCTNO, SSNO, OSACCTNO             
      *       LS-OSRATE -  00.0000 (PRINCIPAL ONLY)    
      *                    ??.???? (EXACT RATE)        
      *                    99.9999 (NORMAL LOAN RATE)  
      *       LE-TYPE:
      *               'E' - LOAN ENDORSER RECORDS
      *               'R' - RESTRICTED RECORDS
      * REV:
      ******************************************************************
       01  LE-REC.
           03  LE1-KEY.
               05  LE-ACCTNO           PIC 9(8)      COMP.
               05  LE-TYPE             PIC X.
               05  LE-FLAG             PIC X(4).
               05  LE-SSNO             REDEFINES LE-FLAG
                                       PIC 9(9)      COMP.
               05  LE-OSACCTNO         PIC 9(8)      COMP.
      ***********************************************************
      *    LE-TYPE = 'E'
      *
      ***********************************************************
           03  ENDORSER-RECORD.
               05  LE-OSRATE               PIC 999V999   COMP-3.
               05  LE-OSAMT                PIC 9(6)V99   COMP-3.
               05  LE-OSDATE               PIC 9(6)      COMP-3.
               05  LE-AMT                  PIC 9(6)V99   COMP-3.
               05  LE-DATE                 PIC 9(6)      COMP-3.
               05  LE-OSMAKERCD            PIC X.
               05  FILLER                  PIC X(12).

      ***********************************************************
      *    LE-TYPE = 'R'
      *    LE-FLAG VALUES ARE:     D = DUAL LOAN
      *                            P = PAIDOFF LOAN
      *                            R = RENEWAL
      *
      ***********************************************************
           03  RESTRICTED-RECORD REDEFINES ENDORSER-RECORD.
               05  LE-LOANDATE             PIC 9(6)      COMP-3.
               05  LE-LNAMT                PIC 9(7)V99   COMP-3.
               05  LE-POFFDATE             PIC 9(6)      COMP-3.   
               05  LE-RESTRICTED-MONEY     PIC 9(7)V99   COMP-3.
               05  LE-BALANCE              PIC 9(7)V99   COMP-3.
               05  LE-SALEFIN              PIC X.
