      ****************************************************
      * ASCII COLLECTOR JOURNAL FILE AND TAG FILE
      ****************************************************
       01  ACJ-PATH.
           03  ACJ-PATH-BEG           PIC X(12).
           03  ACJ-FILENAME           PIC X(15) VALUE "/ASCII/ACJFILE.".
           03  ACJ-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ACJ-DONE-PATH.
           03  ACJ-DONE-PATH-BEG      PIC X(12).
           03  ACJ-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ACJFILE.".
           03  ACJ-DONE-PATH-GROUP    PIC X(04).
