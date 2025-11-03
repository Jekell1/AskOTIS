      * COPYMEMBER: LIBUT/ZMSCANW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBUT/ZMSCANW
      *
      * DESCRIPTION   :  CODE "AS" SCAN LINK WORKERS
      *
      *=================================================================
      ******************************************************************
      *
      *    U T S C A N - B U F
      *
      ******************************************************************
       03  ZMSCAN-BUF.

           05  ZMSCAN-LINES            PIC 9(02)   VALUE 15.
           05  ZMSCAN-COLUMNS          PIC 9(02)   VALUE 27.
           05  ZMSCAN-BEGIN-Y          PIC 9(02)   VALUE 04.
           05  ZMSCAN-BEGIN-X          PIC 9(02)   VALUE 21.
           05  ZMSCAN-FORM-NAM         PIC X(09)   VALUE "UT/ZMSCAN".

           05  ZMSCAN-WINDOW-BUF. 
               07  ZMSCAN-KEY          PIC X(2).
