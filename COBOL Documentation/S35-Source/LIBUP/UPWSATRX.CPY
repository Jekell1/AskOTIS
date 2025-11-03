      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRX-PATH.
           03  ATRX-PATH-BEG        PIC X(12).
           03  ATRX-FILENAME        PIC X(16) VALUE "/ASCII/ATRXFILE.".
           03  ATRX-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRX-DONE-PATH.
           03  ATRX-DONE-PATH-BEG   PIC X(12).
           03  ATRX-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRXFILE.".
           03  ATRX-DONE-PATH-GROUP PIC X(04).
