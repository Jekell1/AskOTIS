      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRC-PATH.
           03  ATRC-PATH-BEG        PIC X(12).
           03  ATRC-FILENAME        PIC X(16) VALUE "/ASCII/ATRCFILE.".
           03  ATRC-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRC-DONE-PATH.
           03  ATRC-DONE-PATH-BEG   PIC X(12).
           03  ATRC-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRCFILE.".
           03  ATRC-DONE-PATH-GROUP PIC X(04).
