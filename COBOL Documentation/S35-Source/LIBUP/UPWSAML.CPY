      ****************************************************
      * ASCII MAIL LIST FILE AND TAG FILE
      ****************************************************
       01  AML-PATH.
           03  AML-PATH-BEG           PIC X(12).
           03  AML-FILENAME           PIC X(15) VALUE "/ASCII/AMLFILE.".
           03  AML-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  AML-DONE-PATH.
           03  AML-DONE-PATH-BEG      PIC X(12).
           03  AML-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/AMLFILE.".
           03  AML-DONE-PATH-GROUP    PIC X(04).
