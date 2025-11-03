      * COPYMEMBER: LIBUP/UP3234W.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBUP/UP3234.CPY
      *
      * DESCRIPTION   : WORKERS FOR UPGRADE A30 TO A32
      *
      *=================================================================
      * REV:
      * KEC 2018-0918 ***NEW*** LOCAL-TO-GLOBAL L2G
      *
      ******************************************************************
      *
      *    U P 3 2 3 4 - M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  UP3234-RECORD-COUNTERS                          VALUE ZEROES.
           03  UP3234-RECORD-COUNTER-READ      PIC 9(10).
           03  UP3234-RECORD-COUNTER-WRITE     PIC 9(10).
           03  UP3234-RECORD-COUNTER-SKIP      PIC 9(10).

       01  UP3234-REPORT-IFN                   PIC X(06) VALUE "REPORT".

      ******************************************************************
      *
      *    U P 3 2 3 4 - F I L E - W O R K E R S
      *
      ******************************************************************
       01  UP3234-CURRENT-FILE                 PIC X(06)   VALUE SPACES.

       01  UP3234-FILE-INPUT                   PIC X(22)   VALUE SPACES.
       01  FILLER                    REDEFINES UP3234-FILE-INPUT.
           03  UP3234-FILE-INPUT-DIR           PIC X(02).
           03  UP3234-FILE-INPUT-SLASH         PIC X(01).
           03  UP3234-FILE-INPUT-NAME          PIC X(19).

      *-----------------------------------------------------------------
       78  UP3234-FILE-ORDER---SOURCE                      VALUE "S".
       78  UP3234-FILE-ORDER---TARGET                      VALUE "T".
       77  UP3234-FILE-ORDER                   PIC X(01)   VALUE SPACES.
           88  UP3234-FILE-ORDER-SOURCE                    VALUE "S".
           88  UP3234-FILE-ORDER-TARGET                    VALUE "T".

      *-----------------------------------------------------------------
       78  UP3234-FILE-TYPE---GLOBAL                       VALUE "G".
       78  UP3234-FILE-TYPE---LOCAL                        VALUE "L".
       77  UP3234-FILE-TYPE                    PIC X(01)   VALUE SPACES.
           88  UP3234-FILE-TYPE-GLOBAL                     VALUE "G".
           88  UP3234-FILE-TYPE-LOCAL                      VALUE "L".

      *=================================================================
       01  UP3234-UPDATE-KEY           PIC X(40)     VALUE SPACES.

       01  UP3234-UPDATE-STAT          PIC X(05)     VALUE SPACES.
       01  FILLER            REDEFINES UP3234-UPDATE-STAT.
           03  UP3234-UPDATE-STAT-D1   PIC X(02).
           03  UP3234-UPDATE-STAT-C    PIC X(01).
           03  UP3234-UPDATE-STAT-D2   PIC X(02).

       01  UP3234-UPDATE-TYPE          PIC X(01)     VALUE SPACES.
           88  UP3234-UPDATE-TYPE-ERROR              VALUE "E".
           88  UP3234-UPDATE-TYPE-SKIP               VALUE SPACES.

      *=================================================================
       01  UP3234-FILE-PATH                          PIC X(35).

       01  FILLER                          REDEFINES UP3234-FILE-PATH.
           03  FILLER                                PIC X(01).
           03  UP3234-FILE-PATH-FIL                  PIC X(34).

       01  FILLER                          REDEFINES UP3234-FILE-PATH.
           03  UP3234-FILE-PATH-SLASH-1              PIC X(01).
           03  UP3234-FILE-PATH-DIR-1                PIC X(03).
           03  UP3234-FILE-PATH-SLASH-2              PIC X(01).
           03  UP3234-FILE-PATH-DIR-2                PIC X(04).
           03  UP3234-FILE-PATH-SLASH-3              PIC X(01).
           03  UP3234-FILE-PATH-MACHINE              PIC X(02).
           03  UP3234-FILE-PATH-SLASH-4              PIC X(01).
           03  UP3234-FILE-PATH-GLOBAL-LOCAL         PIC X(22).

       01  FILLER                          REDEFINES UP3234-FILE-PATH.
           03  FILLER                                PIC X(13).
           03  UP3234-FILE-PATH-GLOBAL-PATH.
               05  UP3234-FILE-PATH-GLOBAL-DIR       PIC X(02).
               05  UP3234-FILE-PATH-GLOBAL-SLASH     PIC X(01).
               05  UP3234-FILE-PATH-GLOBAL-FILE      PIC X(19).

       01  FILLER                          REDEFINES UP3234-FILE-PATH.
           03  UP3234-FILLER                         PIC X(13).
           03  UP3234-FILE-PATH-LOCAL-DATA           PIC X(06).
           03  UP3234-FILE-PATH-LOCAL-DATA-SL        PIC X(01).
           03  UP3234-FILE-PATH-LOCAL-PATH.
               05  UP3234-FILE-PATH-LOCAL-DIR        PIC X(02).
               05  UP3234-FILE-PATH-LOCAL-SLASH      PIC X(01).
               05  UP3234-FILE-PATH-LOCAL-FILE       PIC X(12).

      ******************************************************************
      *
      *    U P 3 2 3 4 - D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  UP3234-LN-FEED                     PIC 9(03)    VALUE ZEROES.

      *=================================================================
       01  UP3234-DETAIL-LINE                 PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  UP3234-DETAIL-LINE-1     REDEFINES UP3234-DETAIL-LINE.
           03  UP3234-D1-BRANCH-PROMPT        PIC X(08).
           03  UP3234-D1-BRANCH               PIC ZZZ9.
           03  FILLER                         PIC X(02).
           03  UP3234-D1-BRANCH-NAME          PIC X(66).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-2            REDEFINES UP3234-DETAIL-LINE.
           03  UP3234-D2-MACHINE-PROMPT       PIC X(09).
           03  UP3234-D2-MACHINE              PIC X(02).
           03  FILLER                         PIC X(03).
           03  UP3234-D2-MACHINE-NAME         PIC X(66).

      *-----------------------------------------------------------------
       01  UP3234-FILE-LINE-1       REDEFINES UP3234-DETAIL-LINE.
           03  FILLER                         PIC X(08).
           03  UP3234-F1-FILE-PROMPT          PIC X(06).
           03  UP3234-F1-FILE                 PIC X(02).
           03  FILLER                         PIC X(02).
           03  UP3234-F1-FILE-MESSAGE         PIC X(62).
           03  FILLER               REDEFINES UP3234-F1-FILE-MESSAGE.
               05  UP3234-F1-RECORD-READ-PROMPT   PIC X(06).
               05  UP3234-F1-RECORD-READ          PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  UP3234-F1-RECORD-WRITE-PROMPT  PIC X(07).
               05  UP3234-F1-RECORD-WRITE         PIC Z,ZZZ,ZZZ,ZZ9.
               05  FILLER                         PIC X(02).
               05  UP3234-F1-RECORD-SKIP-PROMPT   PIC X(06).
               05  UP3234-F1-RECORD-SKIP          PIC Z,ZZZ,ZZZ,ZZ9.

      ******************************************************************
      *
      *    U P 3 2 3 4 - V P I - W O R K E R S 
      *
      *=================================================================
      *    UP3234 VIDEO PROGRESS INDICATOR (VPI)
      ******************************************************************
       01  UP3234-VPI-BRNO                 PIC 9(04)   VALUE ZEROES.

       01  UP3234-VPI-PRU-LP               PIC X(03)   VALUE "HLD".
           88  UP3234-VPI-QUEUE-PIGGYBACK              VALUE "PRU"
                                                             "DEV".
           88  UP3234-VPI-QUEUE-PRINTER                VALUE "   ".
           88  UP3234-VPI-QUEUE-TERMINAL               VALUE "VDU".
           88  UP3234-VPI-QUEUE-VIEW                   VALUE "HLD".
           88  UP3234-VPI-QUEUE-PRINTER-OR-VIEW        VALUE "   "
                                                             "HLD".

       01  UP3234-VPI-ROUTE-BUF            PIC X(02)   VALUE "00".
           88  UP3234-VPI-ROUTE-MENU-DRIVEN            VALUE "00" "  ".

       01  UP3234-VPI-SLEEP-COUNTER        PIC 9(04)   VALUE ZEROES.
       01  UP3234-VPI-SLEEP-COUNTER-MAX    PIC 9(04)   VALUE 500.
       01  UP3234-VPI-TICKER               PIC X(01)   VALUE "-".
       01  UP3234-VPI-TIME-EDIT            PIC X(08)   VALUE SPACES.

      *=================================================================
      *
      *    U P 3 2 3 4 - V P I - D I S P L A Y  - W O R K E R S
      *
      *=================================================================
       01  UP3234-VPI-DISPLAY-BUF               PIC X(78)  VALUE SPACES.
       01  FILLER                     REDEFINES UP3234-VPI-DISPLAY-BUF.
           03  UP3234-VPI-DISPLAY-LEFT          PIC X(01).
           03  UP3234-VPI-DISPLAY-TICKER        PIC X(01).
           03  UP3234-VPI-DISPLAY-RIGHT         PIC X(01).
           03  FILLER                           PIC X(01).
           03  UP3234-VPI-DISPLAY-TEXT          PIC X(74).
           03  FILLER                 REDEFINES UP3234-VPI-DISPLAY-TEXT.
               05  UP3234-VPI-DISPLAY-BRNO      PIC 9(04).
               05  FILLER                       PIC X(02).
               05  UP3234-VPI-DISPLAY-FILE      PIC X(58).
               05  FILLER                       PIC X(02).
               05  UP3234-VPI-DISPLAY-TIME      PIC X(08).

       01  UP3234-VPI-DISPLAY-LIST.
           03  FILLER PIC X(14) VALUE "LEGEND ACCEPT.".

      *=================================================================
      *
      *    U P 3 2 3 4 - V P I - F I L E - W O R K E R S
      *
      *=================================================================
       78  UP3234-VPI-FILE-TYPE---GLOBAL                 VALUE "G".
       78  UP3234-VPI-FILE-TYPE---LOCAL                  VALUE "L".
       77  UP3234-VPI-FILE-TYPE            PIC X(01)     VALUE SPACES.
           88  UP3234-VPI-FILE-TYPE-GLOBAL               VALUE "G".
           88  UP3234-VPI-FILE-TYPE-LOCAL                VALUE "L".

      *-----------------------------------------------------------------
       01  UP3234-VPI-FILE-PREFIX         PIC X(06)   VALUE SPACES.
           88  UP3234-VPI-FILE-EO                     VALUE "EO".
           88  UP3234-VPI-FILE-FD                     VALUE "FD".
           88  UP3234-VPI-FILE-FDB                    VALUE "FDB".
           88  UP3234-VPI-FILE-SN                     VALUE "SN".
      *- - - - - - - - - - -{ MISC. }- - - - - - - - - - -{ MISC. }- - -
           88  UP3234-VPI-FILE-WK1                    VALUE "WK1".
           88  UP3234-VPI-FILE-WK2                    VALUE "WK2".
           88  UP3234-VPI-FILE-REPORT                 VALUE "REPORT".

