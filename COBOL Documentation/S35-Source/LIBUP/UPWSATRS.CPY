      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRS-PATH.
           03  ATRS-PATH-BEG        PIC X(12).
           03  ATRS-FILENAME        PIC X(16) VALUE "/ASCII/ATRSFILE.".
           03  ATRS-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRS-DONE-PATH.
           03  ATRS-DONE-PATH-BEG   PIC X(12).
           03  ATRS-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRSFILE.".
           03  ATRS-DONE-PATH-GROUP PIC X(04).
