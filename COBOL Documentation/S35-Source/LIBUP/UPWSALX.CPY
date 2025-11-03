      ****************************************************
      * ASCII LOAN PAYMENT TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALX-PATH.
           03  ALX-PATH-BEG           PIC X(12).
           03  ALX-FILENAME           PIC X(15) VALUE "/ASCII/ALXFILE.".
           03  ALX-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALX-DONE-PATH.
           03  ALX-DONE-PATH-BEG      PIC X(12).
           03  ALX-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ALXFILE.".
           03  ALX-DONE-PATH-GROUP    PIC X(04).
