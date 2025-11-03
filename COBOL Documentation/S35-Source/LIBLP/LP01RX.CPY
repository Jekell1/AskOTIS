      * COPYMEMBER: LIBLP/LP01RX
      ******************************************************************
      *
      *    RESCISSION INDEX FILE 
      *
      *  RX1-KEY = RX-BRNO, RX-ACCTNO
      *  RX2-KEY = RX-BRNO, RX-LNAME, RX-FNAME WITH DUPS
      *  RX3-KEY = RX-BRNO, RX-DLNO, RX-INTDATE, RX-LNAME, RX-FNAME WITH DUPS
      *
      *=================================================================
      * REV:
      * KEC 2017-0116 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED RX-NUMBER AND RX-SEQNO SINCE NOT USED ANYMORE.
      * AMM 2019-0412 CHANGED FOR A32
      *  CS 2019-0509 REMOVED ENDING FILLER, NO FUTURE FIELDS
      * KEC 2020-0318 REMOVED '03  RX-MASTER-RECORD.' AND
      *          SHIFT 05 LEVELS TO BE 03 LEVELS.
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  RX-SIZE                            VALUE 170.
       01  RX-REC.
           03  RX1-KEY.
               05  RX-BRNO                    PIC 9(4).
               05  RX-ACCTNO                  PIC 9(8).
           03  RX-DLNO                        PIC 9(4)V99.
      $XFD DATE=YYYYMMDD
           03  RX-INTDATE                     PIC 9(8).
           03  RX-NAME.
               05  RX-LNAME                   PIC X(35).
               05  RX-FNAME                   PIC X(35).

      $XFD DATE=YYYYMMDD
           03  RX-LTOUCH-DATE                 PIC 9(8)      COMP-3.

      $XFD DATE=YYYYMMDD
           03  RX-LOANDATE                    PIC 9(8)      COMP-3.
           03  RX-LOANAMT                     PIC 9(7)V99   COMP-3.
           03  RX-PROCEEDS                    PIC 9(7)V99   COMP-3.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    RX-STATUS ---> I = IN RESCISSION; R = RESCIND.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  RX-STATUS                      PIC X(1).

           03  FILLER                 PIC X(53).
