      ****************************************************
      * ASCII LOAN TRAILER FILE AND TAG FILE
      ****************************************************
       01  ALTA-PATH.
           03  ALTA-PATH-BEG        PIC X(12).
           03  ALTA-FILENAME        PIC X(16) VALUE "/ASCII/ALTAFILE.".
           03  ALTA-PATH-GROUP      PIC X(04).

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ALTA-DONE-PATH.
           03  ALTA-DONE-PATH-BEG   PIC X(12).
           03  ALTA-DONE-FILENAME   PIC X(21) VALUE
                                               "/ASCII/DONE/ALTAFILE.".
           03  ALTA-DONE-PATH-GROUP PIC X(04).
