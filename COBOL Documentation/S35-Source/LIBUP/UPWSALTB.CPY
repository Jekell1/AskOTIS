      ****************************************************
      * ASCII LOAN TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALTB-PATH.
           03  ALTB-PATH-BEG        PIC X(12).
           03  ALTB-FILENAME        PIC X(16) VALUE "/ASCII/ALTBFILE.".
           03  ALTB-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALTB-DONE-PATH.
           03  ALTB-DONE-PATH-BEG   PIC X(12).
           03  ALTB-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALTBFILE.".
           03  ALTB-DONE-PATH-GROUP PIC X(04).
