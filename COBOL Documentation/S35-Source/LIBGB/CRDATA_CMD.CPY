      * COPYMEMBER: LIBGB/CRDATA.CMD
      *=================================================================
      *
      *    L E G E N D - I N F O R M A T I O N
      *
      *=================================================================
       01  LEGEND-TEXT.
           03  LEGEND-BEG-BRANCH.
               05  FILLER             PIC X(40)       VALUE 
                   "F1-CLEAR, F6-GROUP SCAN, F7-EXIT, ENTER ".
               05  FILLER             PIC X(38)       VALUE 
                   "BRANCH NUMBER OR GROUP ID             ".

           03  LEGEND-ENTER-ACTION.
               05  FILLER             PIC X(40)       VALUE 
                   "^/v, F1-CLEAR, F7-EXIT, ENTER ACTION SEQ".
               05  FILLER             PIC X(38)       VALUE 
                   ".                                     ".

           03  LEGEND-BEG-FILE.
               05  FILLER             PIC X(40)       VALUE 
                   "^/v, F1-CLEAR, F7-EXIT, ENTER BEGINNING ".
               05  FILLER             PIC X(38)       VALUE 
                   "FILE NUMBER                           ".

           03  LEGEND-END-FILE.
               05  FILLER             PIC X(40)       VALUE 
                   "^/v, F1-CLEAR, F7-EXIT, ENTER ENDING FIL".
               05  FILLER             PIC X(38)       VALUE 
                   "E NUMBER                              ".

           03  LEGEND-ENTER-ACCEPT.
               05  FILLER             PIC X(40)       VALUE 
                   "^/v, F1-CLEAR, F7-EXIT, ACCEPT RANGE SET".
               05  FILLER             PIC X(38)       VALUE 
                   "TINGS                    ACCEPT (Y/N):".

