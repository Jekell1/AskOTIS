      * COPYMEMBER: LIBAP/AP01TM
      **************************************************
      *  TERMS MASTER FILE
      **************************************************
       01  TM-REC.
           03  TM-NUMBER       PIC 99.
           03  TM-DESC         PIC X(25).
           03  TM-DISCOUNT     PIC 99V99       COMP-3.
           03  TM-DISCDAYS     PIC 9(4)        COMP-3.
           03  TM-NETDAYS      PIC 9(4)        COMP-3.
           03  TM-MONTH        PIC 99          COMP-3.
           03  TM-CUT-OFF      PIC 99          COMP-3.
           03  TM-NO-PAYS      PIC 99          COMP-3.
           03  TM-DEFAULT-FLAG PIC X.
           03  TM-UNUSED       PIC X(20).
