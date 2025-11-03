      ****************************************************
      * ASCII PAYMENT FILE AND TAG FILE
      ****************************************************
       01  ALP-PATH.
           03  ALP-PATH-BEG           PIC X(12).
           03  ALP-FILENAME           PIC X(15) VALUE "/ASCII/ALPFILE.".
           03  ALP-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALP-DONE-PATH.
           03  ALP-DONE-PATH-BEG      PIC X(12).
           03  ALP-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ALPFILE.".
           03  ALP-DONE-PATH-GROUP    PIC X(04).
