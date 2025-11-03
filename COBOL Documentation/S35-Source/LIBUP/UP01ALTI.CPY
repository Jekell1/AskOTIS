      ******************************************************************
      *   ASCII  INSURANCE LOAN TRAILER FILE 
      *  TAB DELMITED
      *
      ******************************************************************
       78 ALTI-SIZE                      VALUE 350.
       01 ALTI-REC.
           03  ALTI-BRNO                  PIC 9(4).
           03  FILLER                     PIC X.
           03  ALTI-ACCTNO                PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTI-SEQNO                 PIC 999.
           03  FILLER                     PIC X.
           03  ALTI-LTOUCH-DATE           PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTI-INS-TYPE              PIC 99.
           03  FILLER                     PIC X.
           03  ALTI-ORIG-1STPYDATE        PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTI-PRIOR-NODEF           PIC 9(3).
           03  FILLER                     PIC X.
           03  ALTI-FEES               OCCURS 2.
               05  ALTI-INSFEECD          PIC XX.
               05  FILLER                 PIC X.
               05  ALTI-INSFEE            PIC +9(7).99.
               05  FILLER                 PIC X.
           03  ALTI-TRCD                  PIC X(2).
           03  FILLER                     PIC X.
           03  ALTI-INSCOMP               PIC 99.
           03  FILLER                     PIC X.
           03  ALTI-INSOURS               PIC X.
           03  FILLER                     PIC X.
           03  ALTI-INSEFF-DATE           PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTI-INSEXP-DATE           PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTI-INSPREM               PIC +9(7).99.
           03  FILLER                     PIC X.
           03  ALTI-INSCOVR               PIC +9(7).99.
           03  FILLER                     PIC X.
           03  ALTI-INSCOMM               PIC +9(7).99.
           03  FILLER                     PIC X.
           03  ALTI-ANTICTERM             PIC 9(3).
           03  FILLER                     PIC X.
           03  ALTI-ANTICTBL           OCCURS 3 TIMES.
               05  ALTI-ANTICERN          PIC +9(7).99.
               05  FILLER                 PIC X.
               05  ALTI-ANTICADJ          PIC +9(7).99.
               05  FILLER                 PIC X.
               05  ALTI-UFERN             PIC +9(7).99.
               05  FILLER                 PIC X.
               05  ALTI-ACCERN            PIC +9(7).99.
               05  FILLER                 PIC X.
           03  ALTI-INT-REBATE            PIC X.
           03  FILLER                     PIC X.
           03  ALTI-INS-PAYMNT-INCR       PIC +9(7).99.
           03  FILLER                     PIC X.
           03  ALTI-INS-TOT-ADDON         PIC +9(7).99.
           03  FILLER                     PIC X.
           03  ALTI-INT-LAST-EARNED       PIC +9(7).99.
           03  FILLER                     PIC X.
           03  ALTI-INS-REBATE            PIC X.
           03  FILLER                     PIC X.
           03  ALTI-RB-WORKER             PIC +9(7).99.
           03  FILLER                     PIC X.
           03  ALTI-ANTIC-START-DATE      PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTI-CANCEL-REASON-CD      PIC X(3).
           03  FILLER                     PIC X.

           03  ALTI-CR                  PIC X.
           03  ALTI-LF                  PIC X.
