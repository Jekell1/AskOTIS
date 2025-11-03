      * COPYMEMBER: LIBCL/CREARXW
      ******************************************************************
      *    CREARXW   RESCISSION INDEX FILE CREATE WORKER
      *
      *
      *    NOTE:
      *          CREARX-ACTION MUST BE THE 1ST FIELD
      *
      * REV:
      ******************************************************************
       01  CREARX-AREA.
           03  CREARX-ACTION                 PIC X.
           03  CREARX-ERRCD                  PIC X.
           03  CREARX-RX-BRNO                PIC 9(4).
           03  CREARX-RX-ACCTNO              PIC 9(8)    COMP-3.
           03  CREARX-RX-DLNO                PIC 9(4)V99 COMP-3.
           03  CREARX-RX-INTDATE             PIC 9(8)    COMP-3.
           03  CREARX-RX-LNAME               PIC X(35).
           03  CREARX-RX-FNAME               PIC X(35).
           03  CREARX-RX-LOANDATE            PIC 9(8)    COMP-3.
           03  CREARX-RX-LOANAMT             PIC 9(7)V99 COMP-3.
           03  CREARX-RX-PROCEEDS            PIC 9(7)V99 COMP-3.

           03  CREARX-RX-STATUS              PIC X.

