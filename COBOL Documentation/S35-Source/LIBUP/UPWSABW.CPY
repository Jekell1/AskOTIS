      ****************************************************
      * ASCII BORROWER FILE AND TAG FILE
      ****************************************************
       01  ABW-PATH.
           03  ABW-PATH-BEG           PIC X(12).
           03  ABW-FILENAME           PIC X(15) VALUE "/ASCII/ABWFILE.".
           03  ABW-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ABW-DONE-PATH.
           03  ABW-DONE-PATH-BEG      PIC X(12).
           03  ABW-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ABWFILE.".
           03  ABW-DONE-PATH-GROUP    PIC X(04).
