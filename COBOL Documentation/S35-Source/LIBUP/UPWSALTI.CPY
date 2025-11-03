      ****************************************************
      * ASCII LOAN TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALTI-PATH.
           03  ALTI-PATH-BEG        PIC X(12).
           03  ALTI-FILENAME        PIC X(16) VALUE "/ASCII/ALTIFILE.".
           03  ALTI-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALTI-DONE-PATH.
           03  ALTI-DONE-PATH-BEG   PIC X(12).
           03  ALTI-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALTIFILE.".
           03  ALTI-DONE-PATH-GROUP PIC X(04).
