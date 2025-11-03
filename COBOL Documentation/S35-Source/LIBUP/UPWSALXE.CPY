      ****************************************************
      * ASCII LOAN PAYMENT TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALXE-PATH.
           03  ALXE-PATH-BEG        PIC X(12).
           03  ALXE-FILENAME        PIC X(16) VALUE "/ASCII/ALXEFILE.".
           03  ALXE-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALXE-DONE-PATH.
           03  ALXE-DONE-PATH-BEG   PIC X(12).
           03  ALXE-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALXEFILE.".
           03  ALXE-DONE-PATH-GROUP PIC X(04).
