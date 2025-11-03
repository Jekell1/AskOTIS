      ****************************************************
      * ASCII GENERAL LEDGER FILE AND TAG FILE
      ****************************************************
       01  AGT-PATH.
           03  AGT-PATH-BEG           PIC X(12).
           03  AGT-FILENAME           PIC X(14) VALUE "/ASCII/AGTFILE".

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  AGT-DONE-PATH.
           03  AGT-DONE-PATH-BEG      PIC X(12).
           03  AGT-DONE-FILENAME      PIC X(19) VALUE
                                                 "/ASCII/DONE/AGTFILE".
