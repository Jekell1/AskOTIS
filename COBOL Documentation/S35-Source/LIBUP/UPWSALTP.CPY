      ****************************************************
      * ASCII LOAN TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALTP-PATH.
           03  ALTP-PATH-BEG        PIC X(12).
           03  ALTP-FILENAME        PIC X(16) VALUE "/ASCII/ALTPFILE.".
           03  ALTP-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALTP-DONE-PATH.
           03  ALTP-DONE-PATH-BEG   PIC X(12).
           03  ALTP-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALTPFILE.".
           03  ALTP-DONE-PATH-GROUP PIC X(04).
