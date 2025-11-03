      ****************************************************
      * ASCII LOAN TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALTC-PATH.
           03  ALTC-PATH-BEG        PIC X(12).
           03  ALTC-FILENAME        PIC X(16) VALUE "/ASCII/ALTCFILE.".
           03  ALTC-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALTC-DONE-PATH.
           03  ALTC-DONE-PATH-BEG   PIC X(12).
           03  ALTC-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALTCFILE.".
           03  ALTC-DONE-PATH-GROUP PIC X(04).
