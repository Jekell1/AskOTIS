      ******************************************************************
      * ASCII LOAN PAYMENT TRAILER EARNINGS FILE: 
      *  TAB DELMITED
      ******************************************************************
       78  ALXE-SIZE                                    VALUE 139.
       01  ALXE-REC.
           03  ALXE-BRNO              PIC 9(4).
           03  FILLER                 PIC X.
           03  ALXE-ACCTNO            PIC 9(8).
           03  FILLER                 PIC X.
           03  ALXE-SEQNO             PIC 9(5).
           03  FILLER                 PIC X.
           03  ALXE-LTOUCH-DATE       PIC 9(8).
           03  FILLER                 PIC X.

           03  ALXE-EARN-TABLE      OCCURS 7.
               05  ALXE-EARN          PIC +9(7).99.
               05  FILLER             PIC X.

           03  ALXE-WORKER            PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXE-WORKER2           PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXE-CR                PIC X.
           03  ALXE-LF                PIC X.
