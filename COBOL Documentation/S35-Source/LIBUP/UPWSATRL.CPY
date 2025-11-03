      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRL-PATH.
           03  ATRL-PATH-BEG        PIC X(12).
           03  ATRL-FILENAME        PIC X(16) VALUE "/ASCII/ATRLFILE.".
           03  ATRL-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRL-DONE-PATH.
           03  ATRL-DONE-PATH-BEG   PIC X(12).
           03  ATRL-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRLFILE.".
           03  ATRL-DONE-PATH-GROUP PIC X(04).
