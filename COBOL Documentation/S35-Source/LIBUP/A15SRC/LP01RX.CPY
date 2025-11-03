      * COPYMEMBER: LIBLP/LP01RX
      ******************************************************************
      *
      *    RESCISSION INDEX FILE : (73) BYTES
      *
      * REV:
      *
      ******************************************************************
       01  RX-REC.
           03  RX1-KEY.
               05  RX-ACCTNO                       PIC 9(8)      COMP.
               05  FILLER                REDEFINES RX-ACCTNO.
                   09  RX-NUMBER                   PIC 9(6)      COMP.
                   09  RX-SEQNO                    PIC 99        COMP.
           03  RX3-KEY.
               05  RX-DLNO                         PIC 9(4)V99   COMP.
               05  RX-INTDATE                      PIC 9(7)      COMP.
               05  RX2-KEY.
                   07  RX-LNAME                    PIC X(16).
                   07  RX-FNAME                    PIC X(16).

      *-----------------------------------------------------------------
           03  RX-MASTER-RECORD.
               05  RX-LOANDATE                     PIC 9(6)      COMP.
               05  RX-LOANAMT                      PIC 9(6)V99.
               05  RX-PROCEEDS                     PIC 9(6)V99.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    RX-STATUS ---> I = IN RESCISSION; R = RESCIND.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  RX-STATUS                       PIC X(1).

           03  FILLER                              PIC X(10).
