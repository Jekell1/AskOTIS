      * COPYMEMBER: LIBGB/PROGIDW
      ******************************************************************
      *
      *   IDENTIFICATION: LIBGB/PROGIDW
      *
      *   DESCRIPTION   : PROGRESS INDICATOR WORKERS
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 121798 ***NEW***
      * KEC 010799 ADDED 'PROGID-TIME' LOGIC.
      *
      ******************************************************************
      *
      *    P R O G I D - L I N E
      *
      *=================================================================
       01  PROGID-LINE.
           03  FILLER                     PIC X(01)   VALUE SPACES.
           03  PROGID-PARENTHESIS-LEFT    PIC X(01)   VALUE "(".
           03  PROGID-INDICATOR           PIC X(01)   VALUE "-".
           03  PROGID-PARENTHESIS-RIGHT   PIC X(01)   VALUE ")".
           03  FILLER                     PIC X(01)   VALUE SPACES.
           03  PROGID-LINE-TEXT           PIC X(75)   VALUE SPACES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  PROGID-BRANCH-LINE         REDEFINES   PROGID-LINE-TEXT.
               05  PROGID-BRANCH-PROMPT   PIC X(08).
               05  PROGID-BRANCH-NUMBER   PIC 9(04).
               05  PROGID-BRANCH-HYPHEN   PIC X(03).
               05  PROGID-BRANCH-TEXT     PIC X(60).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  PROGID-TIME-LINE           REDEFINES   PROGID-LINE-TEXT.
               05  FILLER                 PIC X(66).
               05  PROGID-TIME-FILLER     PIC X(01).
               05  PROGID-TIME-TIME       PIC X(08).

      *=================================================================
      *
      *    P R O G I D - W O R K E R S
      *
      *=================================================================
       01  PROGID-BRANCH                  PIC 9(04)   VALUE ZEROES.
       01  PROGID-FIRST-TIME              PIC X(01)   VALUE "Y".
       01  PROGID-LEVEL                   PIC 9(02)   VALUE ZEROES.
       01  PROGID-LINE-LIST               PIC X(80)   VALUE "LEGEND:B.".

       01  PROGID-PRU-LP                  PIC X(03)   VALUE "HLD".
           88  PROGID-QUEUE-PIGGYBACK                 VALUE "PRU" "DEV".
           88  PROGID-QUEUE-PRINTER                   VALUE "   ".
           88  PROGID-QUEUE-TERMINAL                  VALUE "VDU".
           88  PROGID-QUEUE-VIEW                      VALUE "HLD".
           88  PROGID-QUEUE-PRINTER-OR-VIEW           VALUE "   " "HLD".

       01  PROGID-ROUTE-BUF               PIC X(02)   VALUE "00".
           88  PROGID-ROUTE-MENU-DRIVEN               VALUE "00" "  ".

       01  PROGID-SLEEP-COUNTER           PIC 9(04)   VALUE ZEROES.
       01  PROGID-SLEEP-COUNTER-MAX       PIC 9(04)   VALUE 250.
       01  PROGID-TEXT                    PIC X(75)   VALUE SPACES.
       01  PROGID-TIME                    PIC X(08)   VALUE SPACES.

       01  PROGID-TYPE                    PIC X(01)   VALUE SPACES.
           88  PROGID-TYPE-BRANCH                     VALUE "B".
           88  PROGID-TYPE-STANDARD                   VALUE "S" " ".

      *=================================================================
      *
      *    P R O G I D - T E X T
      *
      *=================================================================
       01  PROGID-TEXT-CONVERTING.
           03  FILLER PIC X(30) VALUE "CONVERTING....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(15) VALUE "...............".

       01  PROGID-TEXT-FIXING.
           03  FILLER PIC X(30) VALUE "FIXING........................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(15) VALUE "...............".

       01  PROGID-TEXT-PRINTING.
           03  FILLER PIC X(30) VALUE "PRINTING......................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(15) VALUE "...............".

       01  PROGID-TEXT-PROCESSING.
           03  FILLER PIC X(30) VALUE "PROCESSING....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(15) VALUE "...............".

       01  PROGID-TEXT-SEARCHING.
           03  FILLER PIC X(30) VALUE "SEARCHING.....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(15) VALUE "...............".

       01  PROGID-TEXT-VERIFYING.
           03  FILLER PIC X(30) VALUE "VERIFYING.....................".
           03  FILLER PIC X(30) VALUE "..............................".
           03  FILLER PIC X(15) VALUE "...............".

