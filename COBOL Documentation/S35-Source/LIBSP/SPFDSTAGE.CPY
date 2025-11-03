
       FD  STAGE-FILE
           LABEL RECORDS ARE STANDARD.
       78  STAGE-PD-SIZE              VALUE 2289.
       01  STAGE-REC.
           03  STAGE-KEY.
               05  STAGE-BRNO          PIC 9(4).
               05  STAGE-SSNO          PIC 9(9).
               05  STAGE-CORRELATION   PIC X(12).
               05  STAGE-CAN-LOAN      PIC 9(2).
           03  STAGE-LN-NUMBER         PIC 9(6).
           03  STAGE-PD-REC            PIC X(STAGE-PD-SIZE).
