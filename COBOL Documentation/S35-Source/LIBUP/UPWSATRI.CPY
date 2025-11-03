      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRI-PATH.
           03  ATRI-PATH-BEG        PIC X(12).
           03  ATRI-FILENAME        PIC X(16) VALUE "/ASCII/ATRIFILE.".
           03  ATRI-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRI-DONE-PATH.
           03  ATRI-DONE-PATH-BEG   PIC X(12).
           03  ATRI-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRIFILE.".
           03  ATRI-DONE-PATH-GROUP PIC X(04).
