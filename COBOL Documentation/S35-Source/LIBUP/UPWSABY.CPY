      ****************************************************
      * ASCII BRANCH STATS FILE AND TAG FILE
      ****************************************************
       01  ABYA-PATH.
           03  ABYA-PATH-BEG        PIC X(12).
           03  ABYA-FILENAME        PIC X(15) VALUE "/ASCII/ABYAFILE".
       01  ABYB-PATH.
           03  ABYB-PATH-BEG        PIC X(12).
           03  ABYB-FILENAME        PIC X(15) VALUE "/ASCII/ABYBFILE".

      * TAG FILE WHEN ASCII FILE IS COMPLETED
       01  ABYA-DONE-PATH.
           03  ABYA-DONE-PATH-BEG   PIC X(12).
           03  ABYA-DONE-FILENAME   PIC X(20) VALUE
                                             "/ASCII/DONE/ABYAFILE".
       01  ABYB-DONE-PATH.
           03  ABYB-DONE-PATH-BEG   PIC X(12).
           03  ABYB-DONE-FILENAME   PIC X(20) VALUE
                                             "/ASCII/DONE/ABYBFILE".
