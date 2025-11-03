      * COPYMEMBER: LIBUP/UPFDATRW.CPY
      ******************************************************************
      *
      * DESCRIPTION:  ASCII TRANSACTION FILE [CASH BALANCING RECORD]
      *
      ******************************************************************
       78  ATRW-SIZE                    VALUE 788.
       01  ATRW-REC.
           03  ATW-BRNO             PIC 9(4).
           03  FILLER               PIC X.
           03  ATW-DATE             PIC 9(8).
           03  FILLER               PIC X.
           03  ATW-TILLNO           PIC 9(2).
           03  FILLER               PIC X.
           03  ATW-LTOUCH-DATE      PIC 9(8) .
           03  FILLER               PIC X.
           03  ATW-CLOSE-TABLE    OCCURS 20.
               05  ATW-POSTNAME     PIC X(08).
               05  FILLER           PIC X.
               05  ATW-TIME         PIC 9(04).
               05  FILLER           PIC X.
               05  ATW-AMOUNT       PIC +9(07).99.
               05  FILLER           PIC X.
               05  ATW-OVERSHORT    PIC +9(07).99.
               05  FILLER           PIC X.
           03  ATW-CR                PIC X.
           03  ATW-LF                PIC X.

