      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRB-PATH.
           03  ATRB-PATH-BEG        PIC X(12).
           03  ATRB-FILENAME        PIC X(16) VALUE "/ASCII/ATRBFILE.".
           03  ATRB-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRB-DONE-PATH.
           03  ATRB-DONE-PATH-BEG   PIC X(12).
           03  ATRB-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRBFILE.".
           03  ATRB-DONE-PATH-GROUP PIC X(04).
