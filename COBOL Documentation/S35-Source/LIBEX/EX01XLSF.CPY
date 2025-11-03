      * COPYMEMBER: LIBEX/EX01XLSF
      ******************************************************************
      *    LOAN EXPANDED FOLLOWUP TRAILER
      *
      * REV:
      *  BAH 070313 TRITON PR# 2937  NEW EXPANDED FOLLOWUP INFO LS REC "F"
      ******************************************************************
       78  XLSF-SIZE                      VALUE 168.
       01  XLSF-REC.
           03  XLS-F-BRACCTNO             PIC 9(10).
           03  XLS-F-ACCTNO               PIC 9(8).
           03  XLS-F-CODE                 PIC X.
           03  XLS-F-SEQNO                PIC 9.

           03  XLS-F-SKIP-START-DATE       PIC 9(8).
           03  XLS-F-SKIP-END-DATE         PIC 9(8).
           03  XLS-F-OFR-START-DATE        PIC 9(8).
           03  XLS-F-OFR-END-DATE          PIC 9(8).
           03  XLS-F-IMPOUND-START-DATE    PIC 9(8).
           03  XLS-F-IMPOUND-END-DATE      PIC 9(8).
           03  XLS-F-BNKRPT-START-DATE     PIC 9(8).
           03  XLS-F-BNKRPT-END-DATE       PIC 9(8).
           03  XLS-F-INSUR-START-DATE      PIC 9(8).
           03  XLS-F-INSUR-END-DATE        PIC 9(8).
           03  XLS-F-COLLECT-START-DATE    PIC 9(8).
           03  XLS-F-COLLECT-END-DATE      PIC 9(8).

           03  FILLER                     PIC X(50).
           03  XLS-F-CR                    PIC X.
           03  XLS-F-LF                    PIC X.
