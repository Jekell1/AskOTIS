      ****************************************************
      * ASCII LOAN FILE AND TAG FILE
      ****************************************************
       01  ALN-PATH.
           03  ALN-PATH-BEG           PIC X(12).
           03  ALN-FILENAME           PIC X(15) VALUE "/ASCII/ALNFILE.".
           03  ALN-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALN-DONE-PATH.
           03  ALN-DONE-PATH-BEG      PIC X(12).
           03  ALN-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ALNFILE.".
           03  ALN-DONE-PATH-GROUP    PIC X(04).
