      * COPYMEMBER: LIBGB/ACCESSW
      ******************************************************
      *    ACCESSW:  WORKERS FOR TESTING EXISTENCE OF FILES
      *
      *    LIMIT COMPLETE PATH TO 40 BYTES
      * 120103 ADDED ACCESS-STAT,ACCESS-FILE-INFO FOR USE OF ACU C$FILEINFO
      * 120524 CHANGED TO USE LONGER BUF (SAME AS LACCESSW)
      * 120614 THIS WAS NEVER PUT IN USE FOR A15, IT IS NOW ... CHANGE FOR
      *        PLATFORM CHANGE
      * 121017 ADDED ACCESS-SOURCE & ACCESS-TARGET.
      ******************************************************
      *01  ACCESS-BUF    PIC X(41).

       01  ACCESS-BUF    PIC X(128).
      *NOTE IN LACCESSW SAYS LIMIT COMPLETE PATH TO 127*

       01  ACCESS-STAT                     PIC 99.
       01  ACCESS-FILE-INFO.
          03  ACCESS-SIZE                  PIC X(8) COMP-X.
          03  ACCESS-FILE-DATE             PIC 9(8) COMP-X.
          03  ACCESS-FILE-TIME             PIC 9(8) COMP-X.

