      ****************************************************
      * ASCII TRANS FILE AND TAG FILE
      ****************************************************
       01  ATRW-PATH.
           03  ATRW-PATH-BEG        PIC X(12).
           03  ATRW-FILENAME        PIC X(16) VALUE "/ASCII/ATRWFILE.".
           03  ATRW-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ATRW-DONE-PATH.
           03  ATRW-DONE-PATH-BEG   PIC X(12).
           03  ATRW-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ATRWFILE.".
           03  ATRW-DONE-PATH-GROUP PIC X(04).
