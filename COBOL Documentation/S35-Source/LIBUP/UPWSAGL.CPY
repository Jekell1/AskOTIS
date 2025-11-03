      ****************************************************
      * ASCII GENERAL LEDGER FILE AND TAG FILE
      ****************************************************
       01  AGL-PATH.
           03  AGL-PATH-BEG           PIC X(12).
           03  AGL-FILENAME           PIC X(14) VALUE "/ASCII/AGLFILE".

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  AGL-DONE-PATH.
           03  AGL-DONE-PATH-BEG      PIC X(12).
           03  AGL-DONE-FILENAME      PIC X(19) VALUE
                                                 "/ASCII/DONE/AGLFILE".
