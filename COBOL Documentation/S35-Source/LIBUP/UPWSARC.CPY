      ****************************************************
      * ASCII DAILY STATUS FILE AND TAG FILE
      ****************************************************
       01  ARC-PATH.
           03  ARC-PATH-BEG           PIC X(12).
           03  ARC-FILENAME           PIC X(14) VALUE "/ASCII/ARCFILE".

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ARC-DONE-PATH.
           03  ARC-DONE-PATH-BEG      PIC X(12).
           03  ARC-DONE-FILENAME      PIC X(19) VALUE
                                                 "/ASCII/DONE/ARCFILE".
