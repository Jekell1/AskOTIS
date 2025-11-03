
      *********************************************************
      * LIBGB/BWARGS.CPY
      * USED IN BITWIS PROGRAM
      *
      * VERYANT
      *
      * WORKERS FOR UTILITIES TO PERFORM BITWISE OPERATIONS ON INTEGERS
      *
      * REV:
      * MJD 130510 NEW COPYBOOK FROM VERYANT (BWARGS.CPY).
      *            CHANGED ALL "BW-" TO BE "BWARGS-" TO AVOID CONFUSION
      *            WITH OUR BW-REC FIELDS
      *********************************************************
       01 BWARGS-ARGS.
          05 BWARGS-OPCODE           PIC X(2).
             88 BWARGS-RIGHTSHIFT    VALUE ">>".
             88 BWARGS-LEFTSHIFT     VALUE "<<".
             88 BWARGS-AND           VALUE "&".
             88 BWARGS-OR            VALUE "|".
             88 BWARGS-XOR           VALUE "^".
          05 BWARGS-OP-TYPE          PIC X.
             88 BWARGS-TINT          VALUE "I".
             88 BWARGS-TCHAR         VALUE "C".
          05 BWARGS-POSITIONS        PIC 9(2).
          05 BWARGS-OP1.
             10 BWARGS-OP1-INT       USAGE UNSIGNED-INT.
             10 BWARGS-OP1-CHAR      PIC X(1) REDEFINES BWARGS-OP1-INT.
          05 BWARGS-OP2.
             10 BWARGS-OP2-INT       USAGE UNSIGNED-INT.
             10 BWARGS-CHAR          PIC X(1) REDEFINES BWARGS-OP2-INT.
          05 BWARGS-RESULT.
             10 BWARGS-RES-INT       USAGE UNSIGNED-INT.
             10 BWARGS-RES-CHAR      PIC X.

