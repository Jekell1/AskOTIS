      *-------------------------------------------------------
      * WORKERS FOR COBOL VERSION OF FLDSEL
      *
      * FORKERS FOR CALL-FLDSEL (LIBGB/FLDSEL)
      * MJD 130529 EXPANDED TO ALLOW UP TO 80 SEP SELECTIONS.
      * MJD 140812 ADDED FSEL-FIELD.  NEED FSEL-FIELD TO DETERMINE WHAT
      *            FIELD TO START IN. 
      * MJD 170103 ADDED FSEL-CONTROL. TESTING OUT SOME SEARCH OPTIONS.
      * BAH 2024.0308 REMOVED FSEL-HOLD-COL, FSEL-MAX,  FSEL-ACCEPT-MODIFIER,
      *
      *-------------------------------------------------------
       01  FIELD-SEL-WORKERS.
           03 FSEL-LIST-IN PIC X(320).
           03 FSEL-LIST-INX REDEFINES FSEL-LIST-IN.
              05  FSEL-FLD-INX OCCURS 80 TIMES.
                  07  FSEL-FLD-IN      PIC X(3).
                  07  FILLER           PIC X(1).

           03 FSEL-LIST-OUT PIC X(560).
           03 FSEL-LIST-OUTX REDEFINES FSEL-LIST-OUT.
              05  FSEL-FLD-OUTX OCCURS 80 TIMES.
                  07  FSEL-FLD-OUT      PIC X(3).
                  07  FSEL-COLON-OUT    PIC X(1).
                  07  FSEL-ATTR-OUT     PIC X(1).
                  07  FSEL-SPACE-OUT    PIC X(1).
              05  FSEL-TERM-OUT         PIC X(1).

           03 FSEL-BEG              PIC 9(2).
           03 FSEL-END              PIC 9(2).
           03 FSEL-HOLD-LINE        PIC 9(2).
           03 FSEL-START-LINE       PIC 9(2).
           03 FSEL-CURR             PIC 9(2) VALUE 1.
           03 FSEL-FLD-ALPX.
              05 FSEL-FLD-ALP       PIC X(3).
              05 FILLER             PIC X(1).
           03 FSEL-FLD-NUM          PIC 9(3).
           03 FSEL-LINE-IDX         PIC 9(2).
           03 FSEL-COL-IDX          PIC 9(3).
           03 FSEL-COL              PIC 9(3).
           03 FSEL-EOL              PIC 9(3).
           03 FSEL-MATCH            PIC X(1).
              88 FSEL-MATCH-FOUND                VALUE "Y".
           03 FSEL-CHAR             PIC X(1).
           03 FSEL-ACCEPT-LINE      PIC 9(3).
           03 FSEL-ACCEPT-COL       PIC 9(3).
           03 FSEL-D-LINE           PIC X(81).
           03 FSEL-D-LINEX REDEFINES FSEL-D-LINE.
              05 FSEL-D-CHARX       OCCURS 81 TIMES.
                 07 FSEL-D-CHAR     PIC X(1).
           03 FSEL-FIELD            PIC X(04).
           03 FILLER                REDEFINES FSEL-FIELD.
              05  FSEL-NO           PIC 9(02).
              05  FILLER            PIC X(02).
           03 FSEL-CONTROL.
              05 FSEL-SEARCH-TYPE   PIC X(1).
              88 FSEL-SEARCH-DEFAULT VALUE " ".
              88 FSEL-SEARCH-LINE    VALUE "L".
              05 FSEL-SEARCH-COL-BEGIN  PIC 9(2).
              88 FSEL-SEARCH-COL-BEGIN-VALID VALUE 1 THRU 80.
              05 FSEL-SEARCH-COL-END    PIC 9(2).
              88 FSEL-SEARCH-COL-END-VALID VALUE 1 THRU 80.

