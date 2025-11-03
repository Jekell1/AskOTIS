      ******************************************************************
      *
      * ***NOTE*** : TR-ALL-PATH AND TRANS-PATH ARE IN LIBLP/LPWKPATH
      * ***NOTE*** : 
      * ***NOTE*** : THIS COPY MEMBER HAS THE TR-ALL-PATH AND 
      * ***NOTE*** : TRANS-PATH FROM LIBLP/LPWKPATH PLACED HERE FOR
      * ***NOTE*** : USE IN UP/TR3032.CBL
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    /usr/data/r#/ssBBBB/ED/TR#### (#### = mmyy)
      *-----------------------------------------------------------------
       01  OTR-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  OTR-PATH                   PIC X(40)   VALUE SPACES.
       01  OTR-ALL-PATH.
           03  OTR-ALL-BASE           PIC X(09)   VALUE SPACES.
           03  FILLER                 REDEFINES OTR-ALL-BASE.
               05  OTR-ALL-BASE-USR   PIC X(05).
               05  OTR-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  OTR-ALL-MACHINE        PIC X(02)   VALUE SPACES.
           03  FILLER                 PIC X(01)   VALUE "/".
           03  OTR-ALL-DATA           PIC X(06)   VALUE SPACES.
           03  FILLER                 PIC X(06)   VALUE "/ED/TR".
           03  OTR-DATE-PATH          PIC 9(04)   VALUE ZEROES.
           03  FILLER                 REDEFINES OTR-DATE-PATH.
               05  OTR-ALL-MONTH      PIC 9(02).
               05  OTR-ALL-YEAR       PIC 9(02).
           03  FILLER                 PIC X(11)   VALUE SPACES.

