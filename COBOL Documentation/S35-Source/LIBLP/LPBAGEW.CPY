      * COPYMEMBER: LIBLP/LPBAGEW
      * REV:
      *  130430 BAH ADDED BAGE-AGE-MONTHS FOR SP-INSREJECT-FG "E",
      *                                                        CASHCO  PR# 4261
      *  161025 BAH INCREASED DATE FIELDS TO 9(8)
      *  171025 BLM REMOVED REDEF OF BAGE-DATE, 8 DIGIT NOW
      *****************************************************
      *    BOROWER AGE CALCULATION WORKERS
      *****************************************************
      * INPUT FIELDS:
       01  BAGE-DATE          PIC 9(8) VALUE 0.
       01  BAGE-REFDATE       PIC 9(8) VALUE 0.
       01  BAGE-TODAY         PIC 9(8) VALUE 0.

      * OUTPUT FIELDS:
       01  BAGE-AGE           PIC 9(3) COMP-3.
       01  BAGE-AGE-MONTHS    PIC 9(4) COMP-3.

      * WORKERS:
       01  BAGE-MONTHS        PIC 9999 COMP-3.
