      * COPYMEMBER: LIBGB/TSTCANIDW
      ******************************************************************
      *
      * LIBGB/TSTCANIDW - VARIABLES USED WITH TSTCANID
      *
      * REV:
      *
      ******************************************************************
       01  TCAN-WORD.
           03  TCAN-CHAR        PIC X     OCCURS 4.
       01  TCAN-WORDX           REDEFINES TCAN-WORD.
           03  TCAN-DIGIT       PIC 9     OCCURS 4.
       01  TCAN-WORDY           REDEFINES TCAN-WORD.
           03  TCAN-NUMB        PIC 9(4).
       01  TCAN-ZERO-TEST       PIC X(4) REDEFINES TCAN-WORD.
           88 TCAN-WORD-ZERO    VALUE
           "    " "0000" "0   " "00  " "000 " "   0".
           88 TCAN-WORD-SPACES  VALUE
           "    ".

       01  TCAN-HOLD.
           03  TCAN-HOLD-CHAR   PIC X     OCCURS 4.

       01  TCAN-CNT1            PIC 9(6)  VALUE 0.
       01  TCAN-CNT2            PIC 9(6)  VALUE 0.
       01  TCAN-TYPE            PIC X     VALUE " ".
       01  TCAN-ERR             PIC 9     VALUE 0.
       01  TCAN-SRESULT         PIC 9(4)  VALUE 0.
       01  TCAN-SKIP-CD-VERIFY  PIC X     VALUE "N".
