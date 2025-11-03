      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ACH-PATH.
           03  ACH-PATH-BEG        PIC X(12).
           03  ACH-FILENAME        PIC X(15) VALUE "/ASCII/ACHFILE.".
           03  ACH-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ACH-DONE-PATH.
           03  ACH-DONE-PATH-BEG   PIC X(12).
           03  ACH-DONE-FILENAME   PIC X(20) VALUE
                                               "/ASCII/DONE/ACHFILE.".
           03  ACH-DONE-PATH-GROUP PIC X(04).
