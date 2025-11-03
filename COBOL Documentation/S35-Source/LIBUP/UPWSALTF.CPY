      ****************************************************
      * ASCII LOAN TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALTF-PATH.
           03  ALTF-PATH-BEG        PIC X(12).
           03  ALTF-FILENAME        PIC X(16) VALUE "/ASCII/ALTFFILE.".
           03  ALTF-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALTF-DONE-PATH.
           03  ALTF-DONE-PATH-BEG   PIC X(12).
           03  ALTF-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALTFFILE.".
           03  ALTF-DONE-PATH-GROUP PIC X(04).
