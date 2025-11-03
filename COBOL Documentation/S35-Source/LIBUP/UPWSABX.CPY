      ****************************************************
      * ASCII BORROWER TRAILER FILE AND TAG FILE
      ****************************************************
       01  ABX-PATH.
           03  ABX-PATH-BEG           PIC X(12).
           03  ABX-FILENAME           PIC X(15) VALUE "/ASCII/ABXFILE.".
           03  ABX-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ABX-DONE-PATH.
           03  ABX-DONE-PATH-BEG      PIC X(12).
           03  ABX-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ABXFILE.".
           03  ABX-DONE-PATH-GROUP    PIC X(04).
