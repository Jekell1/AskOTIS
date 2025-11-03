      ****************************************************
      * ASCII LOAN ENDORSER FILE AND TAG FILE
      ****************************************************
       01  ALE-PATH.
           03  ALE-PATH-BEG           PIC X(12).
           03  ALE-FILENAME           PIC X(15) VALUE "/ASCII/ALEFILE.".
           03  ALE-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALE-DONE-PATH.
           03  ALE-DONE-PATH-BEG      PIC X(12).
           03  ALE-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ALEFILE.".
           03  ALE-DONE-PATH-GROUP    PIC X(04).
