      * COPYMEMBER: LIBLP/LHSTATW
      ******************************************************************
      *
      *     LHSTATW
      *
      *=================================================================
      * REV:
      * KEC 2015.0605 REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *               FOR USE WITH MICROFOCUS.
      *
      ******************************************************************
       01  LHS-INDXH                   PIC 9(02).
       01  LHS-STDXH                   PIC 9(01).

       01  LH-LAWCODE-TEMP             PIC 99.
       01  LH-LAWCODE-T                REDEFINES LH-LAWCODE-TEMP.
           03  LH-LAWCODE-TA           PIC X.
           03  LH-LAWCODE-TB           PIC X.
       01  LHS-DISPLAY-LNNO.
           03  LHS-CLASS               PIC 99.
           03  LHS-DASH                PIC X.
           03  LHS-NUMBER              PIC 9(6).
           03  LHS-STATS               OCCURS 7 TIMES.
               05  LHS-STAT            PIC X.
       01  LHS-STATFLAGS-01.
           03  LHS-STATFLAGS           PIC X(10).
           03  LHS-STATUSFLAGS         REDEFINES LHS-STATFLAGS
                                       OCCURS 10 TIMES.
               05  LHS-STATFG          PIC X.

