      * COPYMEMBER: LIBUP/UPFDTRD.CPY
      ******************************************************************
      *
      * DESCRIPTION:  ASCII DAILY ATRANSACTION FILE [DEPOSIT]
      *
      * FILE INFO  :  PRIMARY LAYOUT IN LIBLP/LPFDTR.CPY
      *               ATR1-CODE = D - DEPOSIT
      *
      ******************************************************************
       78  ATRD-SIZE                    VALUE 124.
       01  ATRD-REC.
           03  ATD-BRNO             PIC 9(4).
           03  FILLER              PIC X.
           03  ATD-DATE             PIC 9(8).
           03  FILLER              PIC X.
           03  ATD-LTOUCH-DATE      PIC 9(8).
           03  FILLER              PIC X.
           03  ATD-DEPOSIT          PIC +9(7).99.
           03  FILLER              PIC X.
           03  ATD-CHECK-DEPOSIT    PIC +9(7).99.
           03  FILLER              PIC X.
           03  ATD-BANK             PIC 9(11).
           03  FILLER              PIC X.
           03  ATD-CASH             PIC 9(11).
           03  FILLER              PIC X.
           03  ATD-GLFG             PIC X.
           03  FILLER              PIC X.
           03  ATD-GLOVERSHORT      PIC 9(11).
           03  FILLER              PIC X.
           03  ATD-OVERSHORT        PIC +9(5).99.
           03  FILLER              PIC X.
           03  ATD-USERID           PIC X(10).
           03  FILLER              PIC X.
           03  ATD-POSTDATE         PIC 9(8).
           03  FILLER              PIC X.
           03  ATD-POSTTIME         PIC 9(6).
           03  FILLER              PIC X.
           03  ATD-CR              PIC X.
           03  ATD-LF              PIC X.
