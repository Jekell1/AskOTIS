      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRP-PATH.
           03  ATRP-PATH-BEG        PIC X(12).
           03  ATRP-FILENAME        PIC X(16) VALUE "/ASCII/ATRPFILE.".
           03  ATRP-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRP-DONE-PATH.
           03  ATRP-DONE-PATH-BEG   PIC X(12).
           03  ATRP-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRPFILE.".
           03  ATRP-DONE-PATH-GROUP PIC X(04).
