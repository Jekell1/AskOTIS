      * COPYMEMBER: LIBFP/PSTPMCW
      ******************************************************************
      *
      * IDENTIFICATION: LIBFP/PSTPMCW
      *
      * DESCRIPTION   : LASER FORM "DOC DESIGN LETTER"
      *                 - LINK WORKERS
      *
      *=================================================================
      * REV:
      * KEC 2007.1010 {PR#03075} REGENCY FINANCE <A15> [LORI MCBEE]
      *               ***NEW***
      *
      ******************************************************************
       01  PSTPMC-LINK-WORKERS.

           03  PSTPMC-FORM-STAT        PIC X(02)    VALUE "00".
               88  PSTPMC-FORM-STAT-GOOD            VALUE "00".
               88  PSTPMC-FORM-STAT-BAD             VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  PSTPMC-FORM-PAGE-MAX    PIC 9(02)    VALUE 01.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  PSTPMC-FORM-FILE.
               05  FILLER              PIC X(01)    VALUE "/".
               05  PSTPMC-FORM-RELEASE PIC X(07)    VALUE "USR/A##".
               05  FILLER              PIC X(05)    VALUE "/PST/".
               05  PSTPMC-FORM-PROGRAM PIC X(06)    VALUE "PST###".
               05  FILLER              PIC X(01)    VALUE ".".
               05  PSTPMC-FORM-PAGE-NO PIC 9(02)    VALUE ZEROES.
      *KC----> 05  FILLER              PIC X(04)    VALUE ".PST".
