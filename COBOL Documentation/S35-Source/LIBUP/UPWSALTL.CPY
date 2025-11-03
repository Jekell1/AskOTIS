      ****************************************************
      * ASCII LOAN TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALTL-PATH.
           03  ALTL-PATH-BEG        PIC X(12).
           03  ALTL-FILENAME        PIC X(16) VALUE "/ASCII/ALTLFILE.".
           03  ALTL-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALTL-DONE-PATH.
           03  ALTL-DONE-PATH-BEG   PIC X(12).
           03  ALTL-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALTLFILE.".
           03  ALTL-DONE-PATH-GROUP PIC X(04).
