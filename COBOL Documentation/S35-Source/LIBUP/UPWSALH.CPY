      ****************************************************
      * ASCII LOAN HISTORY FILE AND TAG FILE
      ****************************************************
       01  ALH-PATH.
           03  ALH-PATH-BEG           PIC X(12).
           03  ALH-FILENAME           PIC X(15) VALUE "/ASCII/ALHFILE.".
           03  ALH-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALH-DONE-PATH.
           03  ALH-DONE-PATH-BEG      PIC X(12).
           03  ALH-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ALHFILE.".
           03  ALH-DONE-PATH-GROUP    PIC X(04).
