      * COPYMEMBER: LIBLP/UPERTST.CMD
      ***************************************************************
      *    UPERTSTW
      *    TEST UNIT PERIOD CODE AND FREQ WORKERS
      *      OR ESTABLISH DISPLAY MESSAGE
      * REV:
      ***************************************************************
       01  UPERTST-WORK-AREA.
           03  UPERTST-INPUT.
               05  UPERTST-UNITPER-CD    PIC X.
               05  UPERTST-UNITPER-FREQ  PIC 999.
           03  UPERTST-OUTPUT.
               05  UPERTST-ERRCD         PIC X.

       01  UPERTST-MESS.
           03  FILLER                    PIC X(15) VALUE
               "PAYMENTS EVERY ".
           03  UPERTST-MESSX.
               05  UPERTST-MESS-FREQ     PIC ZZ9.
               05  FILLER                PIC X.
               05  UPERTST-MESS-CODE     PIC X(11).

       01  UPERTST-FULL-MESS.
           03  FILLER                    PIC X(40) VALUE
               "M-MONTHLY W-WEEKLY B-BI WEEKLY S-SEMI MO".
           03  FILLER                    PIC X(20) VALUE
               "NTHLY Y-YEARLY D-DAY".
