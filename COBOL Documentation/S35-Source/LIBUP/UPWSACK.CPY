      ****************************************************
      * ASCII CHECK FILE AND TAG FILE
      ****************************************************
       01  ACK-PATH.
           03  ACK-PATH-BEG           PIC X(12).
           03  ACK-FILENAME           PIC X(15) VALUE "/ASCII/ACKFILE.".
           03  ACK-PATH-GROUP         PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ACK-DONE-PATH.
           03  ACK-DONE-PATH-BEG      PIC X(12).
           03  ACK-DONE-FILENAME      PIC X(20) VALUE
                                                 "/ASCII/DONE/ACKFILE.".
           03  ACK-DONE-PATH-GROUP    PIC X(04).
