      * COPYMEMBER: LIBWI/SFINPCW
      * REV:
      *  JTG 032498 ADDED SFINPC-ORGTERM TO CAPTURE SINGLE PAY LOAN
      *  2024.0418 BAH REMOVED SFINPC-PROCEEDS-FG
      *  2024.0506 BAH MADE LEVEL 01, CALL NOT WINDOW
      *----------------------------------------------------------------
       01  SFINPC-BUF.
           03  SFINPC-CALC-SELECT-KEY  PIC X.
           03  SFINPC-REGPYAMT-FG      PIC X.
           03  SFINPC-BALLOON-FG       PIC X.
           03  SFINPC-BALLOON-LAST-FG  PIC X.
           03  SFINPC-SELLPR-FG        PIC X.
           03  SFINPC-DOWNPY-FG        PIC X.
           03  SFINPC-PERDOWN-FG       PIC X.
           03  SFINPC-SELLPR           PIC S9(7)V99  COMP-3.
           03  SFINPC-DOWNPY           PIC S9(7)V99  COMP-3.
           03  SFINPC-PERDOWN          PIC S9(7)V99  COMP-3.
           03  SFINPC-LASTPYAMT        PIC S9(7)V99  COMP-3.
           03  SFINPC-REGPYAMT         PIC S9(7)V99  COMP-3.
           03  SFINPC-ORGTERM          PIC 9(3)      COMP-3.
           03  SFINPC-STATUS           PIC XX.
