      ****************************************
      *A30 CONVERSION  STARTED
      *   GETPRW - PRINTER CONTROL WORKERS
      *   BEFORE MODIFICATION, SEE GB/TSTPRU
      *   GB/QUEUE, LIBGB/MKTEMPW, H_CMD.C
      ****************************************
      *01  PRU-POP-UP-WINDOW   PIC X(10).
       01  PRU-PATH            VALUE " ".
           03  PRU-CMD.
               05  PRU-LP      PIC X(3).
                   88  QUEUE-PIGGYBACK     VALUE "PRU" "DEV".
                   88  QUEUE-PRINTER       VALUE "   ".
                   88  QUEUE-TERMINAL      VALUE "VDU".
                   88  QUEUE-VIEW          VALUE "HLD".
               05  PRU-DASHD   PIC X(2).
               05  PRU-DEST    PIC X(10).
               05  FILLER      PIC X.
               05  PRU-DASHS   PIC X(2).
               05  FILLER      PIC X.
               05  PRU-DASHN   PIC X(2).
               05  PRU-COPIES  PIC 9(1).
               05  FILLER      PIC X(1).
      * USER FIELD NOT USED ON 3B2
               05  PRU-DASHA   PIC X(2).
               05  PRU-USER    PIC X(11).
           03  PRU-FILE.
               05  PRU-DIR     PIC X(17).
               05  PRU-PROG    PIC X(6).
               05  PRU-X       PIC X(6).
               05  FILLER      PIC X(25).
