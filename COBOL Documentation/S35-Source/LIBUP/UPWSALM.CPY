      ****************************************************
      * ASCII LOAN MEMO FILE AND TAG FILE
      ****************************************************
       01  ALM-PATH.
           03  ALM-PATH-BEG           PIC X(12).
           03  ALM-FILENAME           PIC X(15) VALUE "/ASCII/ALMFILE.".
           03  ALM-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALM-DONE-PATH.
           03  ALM-DONE-PATH-BEG      PIC X(12).
           03  ALM-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ALMFILE.".
           03  ALM-DONE-PATH-GROUP    PIC X(04).
