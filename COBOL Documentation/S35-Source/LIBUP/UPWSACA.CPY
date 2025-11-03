      * UPWSACA
      ****************************************************
      * ASCII CASH DRAWER FILE AND TAG FILE
      ****************************************************
       01  ACA-PATH.
           03  ACA-PATH-BEG           PIC X(12).
           03  ACA-FILENAME           PIC X(15) VALUE "/ASCII/ACAFILE.".
           03  ACA-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ACA-DONE-PATH.
           03  ACA-DONE-PATH-BEG      PIC X(12).
           03  ACA-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ACAFILE.".
           03  ACA-DONE-PATH-GROUP    PIC X(04).
