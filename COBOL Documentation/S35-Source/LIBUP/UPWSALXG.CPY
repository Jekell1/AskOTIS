      ****************************************************
      * ASCII LOAN PAYMENT TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALXG-PATH.
           03  ALXG-PATH-BEG        PIC X(12).
           03  ALXG-FILENAME        PIC X(16) VALUE "/ASCII/ALXGFILE.".
           03  ALXG-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALXG-DONE-PATH.
           03  ALXG-DONE-PATH-BEG   PIC X(12).
           03  ALXG-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALXGFILE.".
           03  ALXG-DONE-PATH-GROUP PIC X(04).
