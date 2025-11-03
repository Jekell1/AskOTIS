      ******************************************************************
      *
      *    S Q L - S E T - D A T E
      *
      *=================================================================
      * IN  : SQL-DATE-YYYYMMDD
      * OUT : SQL-DATE-YYYY-MM-DD
      * DESC: CONVERT DATE FROM YYYYMMDD TO YYYY-MM-DD FORMAT;
      *       NEEDED FOR 'SET' FIELDS ROUTINES.
      ******************************************************************
       SQL-SET-DATE.

           IF ( SQL-DATE-YYYYMMDD = ZEROES )
              MOVE "1900-01-01" TO SQL-DATE-YYYY-MM-DD
           ELSE
              MOVE SQL-DATE-YYYYMMDD TO SQL-DATE-YYYY-MM-DD-NUM
              INSPECT SQL-DATE-YYYY-MM-DD-NUM REPLACING ALL "/" BY "-".

      ******************************************************************
      *
      *    S Q L - T E X T - D E C R E M E N T
      *
      *=================================================================
      * IN  : SQL-TEXT
      * OUT : SQL-TEXT
      * DESC: USED TO DECREMENT THE LAST CHARACTER OF A STRING SO THAT
      *       A START-NOT-GREATER PARAGRAPH (DECLARE CURSOR) WORKS LIKE
      *       A VISION FILE IN A MULTI-PAGE SCAN WINDOW;
      *       FIND THE LAST CHARACTER IN A TEXT STRING THEN DECREMENT
      *       IT BY ONE.  USING SQL-TEXT-ASCII WORKERS, WORK BACKWARDS:
      *
      *         " "     BECOMES " " (REMAINS SPACE; FIRST CHARACTER)
      *         "!"     BECOMES " "
      *         "1"     BECOMES "0"
      *         "B"     BECOMES "A" (UPPER B AND A)
      *         "~"     BECOMES "}"
      *
      *         "   "   BECOMES "   " (REMAINS SPACES; NO WHERE TO GO)
      *         "!  "   BECOMES "   "
      *         " ! "   BECOMES "   "
      *         "012"   BECOMES "011"
      *         "ABC"   BECOMES "ABB" (UPPER)
      *         "~~ "   BECOMES "~} "
      *         "~~!"   BECOMES "~~ "
      *         "~~~"   BECOMES "~~}"
      *
      ******************************************************************
