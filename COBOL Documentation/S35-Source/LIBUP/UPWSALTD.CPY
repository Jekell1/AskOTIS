      ****************************************************
      * ASCII LOAN TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALTD-PATH.
           03  ALTD-PATH-BEG        PIC X(12).
           03  ALTD-FILENAME        PIC X(16) VALUE "/ASCII/ALTDFILE.".
           03  ALTD-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALTD-DONE-PATH.
           03  ALTD-DONE-PATH-BEG   PIC X(12).
           03  ALTD-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALTDFILE.".
           03  ALTD-DONE-PATH-GROUP PIC X(04).
