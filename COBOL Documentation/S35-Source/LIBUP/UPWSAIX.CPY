      ****************************************************
      * ASCII ALTERNATE INDEX FILE AND TAG FILE
      ****************************************************
       01  AIX-PATH.
           03  AIX-PATH-BEG           PIC X(12).
           03  AIX-FILENAME           PIC X(15) VALUE "/ASCII/AIXFILE.".
           03  AIX-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  AIX-DONE-PATH.
           03  AIX-DONE-PATH-BEG      PIC X(12).
           03  AIX-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/AIXFILE.".
           03  AIX-DONE-PATH-GROUP    PIC X(04).
