      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRJ-PATH.
           03  ATRJ-PATH-BEG        PIC X(12).
           03  ATRJ-FILENAME        PIC X(16) VALUE "/ASCII/ATRJFILE.".
           03  ATRJ-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRJ-DONE-PATH.
           03  ATRJ-DONE-PATH-BEG   PIC X(12).
           03  ATRJ-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRJFILE.".
           03  ATRJ-DONE-PATH-GROUP PIC X(04).
