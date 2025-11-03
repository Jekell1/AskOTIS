      ****************************************************
      * ASCII BORROWER MEMO FILE AND TAG FILE
      ****************************************************
       01  ABM-PATH.
           03  ABM-PATH-BEG           PIC X(12).
           03  ABM-FILENAME           PIC X(15) VALUE "/ASCII/ABMFILE.".
           03  ABM-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ABM-DONE-PATH.
           03  ABM-DONE-PATH-BEG      PIC X(12).
           03  ABM-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ABMFILE.".
           03  ABM-DONE-PATH-GROUP    PIC X(04).
