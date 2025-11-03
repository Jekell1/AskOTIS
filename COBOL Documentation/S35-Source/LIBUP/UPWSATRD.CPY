      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRD-PATH.
           03  ATRD-PATH-BEG        PIC X(12).
           03  ATRD-FILENAME        PIC X(16) VALUE "/ASCII/ATRDFILE.".
           03  ATRD-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRD-DONE-PATH.
           03  ATRD-DONE-PATH-BEG   PIC X(12).
           03  ATRD-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRDFILE.".
           03  ATRD-DONE-PATH-GROUP PIC X(04).
