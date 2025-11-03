      * COPYMEMBER: LIBWI/WIMESSW
      ******************************************************************
      * DESC:  WORKERS FOR LIBWI/WIMESS, DISPLAY MSG WINDOW
      ******************************************************************
       01  WIMESS-WORKERS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'JUSTIFY' WILL JUSTIFY AUTOMATICALLY CENTER UNLESS SET TO:
      *      "C" = CENTER
      *      "L" = LEFT
      *      "R" = RIGHT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  WIMESS-JUSTIFY-BUF.
               05  WIMESS-JUSTIFY1         PIC X(01).
               05  WIMESS-JUSTIFY2         PIC X(01).
               05  WIMESS-JUSTIFY3         PIC X(01).
               05  WIMESS-JUSTIFY4         PIC X(01).
               05  WIMESS-JUSTIFY5         PIC X(01).
               05  WIMESS-JUSTIFY6         PIC X(01).
               05  WIMESS-JUSTIFY7         PIC X(01).
               05  WIMESS-JUSTIFY8         PIC X(01).
               05  WIMESS-JUSTIFY9         PIC X(01).
           03  FILLER                      REDEFINES WIMESS-JUSTIFY-BUF.
               05  WIMESS-JUSTIFY          PIC X(01) OCCURS 9 TIMES.
                   88  WIMESS-JUSTIFY-CENTER VALUE " " THRU "K"
                                                       "M" THRU "Q"
                                                       "S" THRU "~".
                   88  WIMESS-JUSTIFY-LEFT   VALUE "L".
                   88  WIMESS-JUSTIFY-RIGHT  VALUE "R".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  WIMESS-SUB                   PIC 99.
           03  WIMESS-LINES                 PIC 9 VALUE 9.
           03  WIMESS-DISPLAY-BUF           PIC X(549).
           03  WIMESS-BUF.
               05  WIMESS1                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
               05  WIMESS2                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
               05  WIMESS3                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
               05  WIMESS4                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
               05  WIMESS5                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
               05  WIMESS6                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
               05  WIMESS7                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
               05  WIMESS8                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
               05  WIMESS9                  PIC X(60).
               05  FILLER                   PIC X VALUE X"0A".
           03  FILLER                       REDEFINES WIMESS-BUF.
               05  WIMESS                   OCCURS 9 TIMES.
                   07  WIMESS-MSG           PIC X(60).
                   07  WIMESS-LINE-FD       PIC X.
