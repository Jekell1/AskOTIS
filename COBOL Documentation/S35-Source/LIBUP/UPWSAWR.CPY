      ****************************************************
      * ASCII RATING SHEET FILE AND TAG FILE
      ****************************************************
       01  AWR-PATH.
           03  AWR-PATH-BEG           PIC X(12).
           03  AWR-FILENAME           PIC X(14) VALUE "/ASCII/AWRFILE".

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  AWR-DONE-PATH.
           03  AWR-DONE-PATH-BEG      PIC X(12).
           03  AWR-DONE-FILENAME      PIC X(19) VALUE
                                                 "/ASCII/DONE/AWRFILE".
