      * COPYMEMBER: LIBGB/GETDAT
      * BAH 170323 ACTIVATE 8 DIGIT DATES, PD0006
      *************************
      *  GET SYSTEM DATE
      *************************
       GETDAT.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYS-DATE.
