      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRK-PATH.
           03  ATRK-PATH-BEG        PIC X(12).
           03  ATRK-FILENAME        PIC X(16) VALUE "/ASCII/ATRKFILE.".
           03  ATRK-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRK-DONE-PATH.
           03  ATRK-DONE-PATH-BEG   PIC X(12).
           03  ATRK-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRKFILE.".
           03  ATRK-DONE-PATH-GROUP PIC X(04).
