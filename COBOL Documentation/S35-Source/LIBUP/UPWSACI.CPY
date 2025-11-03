      ****************************************************
      * ASCII CUSTOMER INFO FILE AND TAG FILE
      ****************************************************
       01  ACI-PATH.
           03  ACI-PATH-BEG           PIC X(12).
           03  ACI-FILENAME           PIC X(15) VALUE "/ASCII/ACIFILE.".
           03  ACI-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ACI-DONE-PATH.
           03  ACI-DONE-PATH-BEG      PIC X(12).
           03  ACI-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ACIFILE.".
           03  ACI-DONE-PATH-GROUP    PIC X(04).
