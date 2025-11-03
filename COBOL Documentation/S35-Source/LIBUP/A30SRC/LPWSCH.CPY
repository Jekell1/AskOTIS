      ******************************************************************
      *
      * ***NOTE*** : OCH-ALL-PATH AND OCHANGE-PATH ARE IN LIBLP/LPWKPATH
      * ***NOTE*** : 
      * ***NOTE*** : THIS COPY MEMBER HAS THE OCH-ALL-PATH AND 
      * ***NOTE*** : OCHANGES-PATH FROM LIBLP/LPWKPATH PLACED HERE FOR
      * ***NOTE*** : USE IN UP/TR3032.CBL
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    /usr/data/r#/ssBBBB/ED/OCH#### (#### = mmyy)
      *-----------------------------------------------------------------
       01  OCH-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  OCH-PATH                   PIC X(50)   VALUE SPACES.
       01  OCH-ALL-PATH.
           03  OCH-ALL-BASE           PIC X(09)   VALUE SPACES.
           03  FILLER                 REDEFINES OCH-ALL-BASE.
               05  OCH-ALL-BASE-USR   PIC X(05).
               05  OCH-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  OCH-ALL-MACHINE        PIC XX.
           03  FILLER                 PIC X       VALUE "/".
           03  OCH-ALL-DATA           PIC X(6).
           03  FILLER                 PIC X(6)    VALUE "/ED/CH".
           03  OCH-DATE-PATH          PIC 9(4).
           03  FILLER                 REDEFINES OCH-DATE-PATH.
               05  OCH-ALL-MONTH      PIC 99.
               05  OCH-ALL-YEAR       PIC 99.

