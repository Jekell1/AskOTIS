      * COPYMEMBER: LIBLP/LPSTATW
      ******************************************************************
      *
      *     LPSTATW
      *
      *=================================================================
      * REV:
      * KEC 2015.0604 REMOVED THE USE OF 'INDEXED BY' ON OCCURS FOR USE
      *               WITH MICROFOCUS.
      ******************************************************************
       01  LPS-LAWCODE-TEMP.
           03  LPS-LAWCODE-TA       PIC X.
           03  LPS-LAWCODE-TB       PIC X.
       01  LPS-DISPLAY-LNNO.
           03  LPS-CLASS           PIC 99.
           03  LPS-DASH            PIC X.
           03  LPS-NUMBER          PIC 9(6).
           03  LPS-7STATUS.
               05  LPS-STATS      OCCURS 7.
                   07  LPS-STAT    PIC X.
       01  ACU-CHANGE1.
           03  LPS-STATFLAGS       PIC X(10).
           03  LPS-STATUSFLAGS    REDEFINES LPS-STATFLAGS
                                  OCCURS 10.
               05  LPS-STATFG      PIC X.

       01  LPS-INDX                PIC 9(02).
