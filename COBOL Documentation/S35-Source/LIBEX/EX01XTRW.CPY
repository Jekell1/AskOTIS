      * COPYMEMBER: LIBEX/EX01XTRW
      **************************************************************************
      * CASH DRAWER BALANCING TRANSACTION EXTRACTION FILE      696 BYTES
      * 
      * REV:
      *  2021.0804 NEW #1509
      **************************************************************************
       01  XTRW-REC.
           03  XTRW-BRNO                 PIC 9(04).
           03  XTRW-TRANS-DATE           PIC 9(08).
           03  XTRW-CODE                 PIC X(01).
           03  XTRW-TILLNO               PIC X.
           03  XTRW-XCLOSE-XTABLE.
               05  XTRW-POSTNAME1        PIC X(08).
               05  XTRW-TIME1            PIC 9(04).
               05  XTRW-AMOUNT1          PIC +9(07).99.
               05  XTRW-OVERSHORT1       PIC +9(07).99.
               05  XTRW-POSTNAME2        PIC X(08).
               05  XTRW-TIME2            PIC 9(04).
               05  XTRW-AMOUNT2          PIC +9(07).99.
               05  XTRW-OVERSHORT2       PIC +9(07).99.
               05  XTRW-POSTNAME3        PIC X(08).
               05  XTRW-TIME3            PIC 9(04).
               05  XTRW-AMOUNT3          PIC +9(07).99.
               05  XTRW-OVERSHORT3       PIC +9(07).99.
               05  XTRW-POSTNAME4        PIC X(08).
               05  XTRW-TIME4            PIC 9(04).
               05  XTRW-AMOUNT4          PIC +9(07).99.
               05  XTRW-OVERSHORT4       PIC +9(07).99.
               05  XTRW-POSTNAME5        PIC X(08).
               05  XTRW-TIME5            PIC 9(04).
               05  XTRW-AMOUNT5          PIC +9(07).99.
               05  XTRW-OVERSHORT5       PIC +9(07).99.
               05  XTRW-POSTNAME6        PIC X(08).
               05  XTRW-TIME6            PIC 9(04).
               05  XTRW-AMOUNT6          PIC +9(07).99.
               05  XTRW-OVERSHORT6       PIC +9(07).99.
               05  XTRW-POSTNAME7        PIC X(08).
               05  XTRW-TIME7            PIC 9(04).
               05  XTRW-AMOUNT7          PIC +9(07).99.
               05  XTRW-OVERSHORT7       PIC +9(07).99.
               05  XTRW-POSTNAME8        PIC X(08).
               05  XTRW-TIME8            PIC 9(04).
               05  XTRW-AMOUNT8          PIC +9(07).99.
               05  XTRW-OVERSHORT8       PIC +9(07).99.
               05  XTRW-POSTNAME9        PIC X(08).
               05  XTRW-TIME9            PIC 9(04).
               05  XTRW-AMOUNT9          PIC +9(07).99.
               05  XTRW-OVERSHORT9       PIC +9(07).99.
               05  XTRW-POSTNAME10        PIC X(08).
               05  XTRW-TIME10            PIC 9(04).
               05  XTRW-AMOUNT10          PIC +9(07).99.
               05  XTRW-OVERSHORT10       PIC +9(07).99.
               05  XTRW-POSTNAME11        PIC X(08).
               05  XTRW-TIME11            PIC 9(04).
               05  XTRW-AMOUNT11          PIC +9(07).99.
               05  XTRW-OVERSHORT11       PIC +9(07).99.
               05  XTRW-POSTNAME12        PIC X(08).
               05  XTRW-TIME12            PIC 9(04).
               05  XTRW-AMOUNT12          PIC +9(07).99.
               05  XTRW-OVERSHORT12       PIC +9(07).99.
               05  XTRW-POSTNAME13        PIC X(08).
               05  XTRW-TIME13            PIC 9(04).
               05  XTRW-AMOUNT13          PIC +9(07).99.
               05  XTRW-OVERSHORT13       PIC +9(07).99.
               05  XTRW-POSTNAME14        PIC X(08).
               05  XTRW-TIME14            PIC 9(04).
               05  XTRW-AMOUNT14          PIC +9(07).99.
               05  XTRW-OVERSHORT14       PIC +9(07).99.
               05  XTRW-POSTNAME15        PIC X(08).
               05  XTRW-TIME15            PIC 9(04).
               05  XTRW-AMOUNT15          PIC +9(07).99.
               05  XTRW-OVERSHORT15       PIC +9(07).99.
               05  XTRW-POSTNAME16        PIC X(08).
               05  XTRW-TIME16            PIC 9(04).
               05  XTRW-AMOUNT16          PIC +9(07).99.
               05  XTRW-OVERSHORT16       PIC +9(07).99.
               05  XTRW-POSTNAME17        PIC X(08).
               05  XTRW-TIME17            PIC 9(04).
               05  XTRW-AMOUNT17          PIC +9(07).99.
               05  XTRW-OVERSHORT17       PIC +9(07).99.
               05  XTRW-POSTNAME18        PIC X(08).
               05  XTRW-TIME18            PIC 9(04).
               05  XTRW-AMOUNT18          PIC +9(07).99.
               05  XTRW-OVERSHORT18       PIC +9(07).99.
               05  XTRW-POSTNAME19        PIC X(08).
               05  XTRW-TIME19            PIC 9(04).
               05  XTRW-AMOUNT19          PIC +9(07).99.
               05  XTRW-OVERSHORT19       PIC +9(07).99.
               05  XTRW-POSTNAME20        PIC X(08).
               05  XTRW-TIME20            PIC 9(04).
               05  XTRW-AMOUNT20          PIC +9(07).99.
               05  XTRW-OVERSHORT20       PIC +9(07).99.
      *-------------------------------------------------
           03  XTRW-CLOSE-TABLE REDEFINES XTRW-XCLOSE-XTABLE OCCURS 20.
               05  XTRW-POSTNAME     PIC X(08).
               05  XTRW-TIME         PIC 9(04).
               05  XTRW-AMOUNT       PIC +9(07).99.
               05  XTRW-OVERSHORT    PIC +9(07).99.
           03  XTRW-CR                   PIC X(01).
           03  XTRW-LF                   PIC X(01).
