      * COPYMEMBER: LIBWI/RVENTW
      **************************************************************************
      *    REVERSE ENTRY WINDOW WORKERS
      * REV:
      *  081095 JTG ADDED RVENT-ALP-AMT
      *  081595 JTG ADDED RVENTX-NO-DEF
      *  081595 JTG ADDED RVENT-PRIOR-PAYAMT
      *  092895 JTG ADDED RVENT-REFNO TO HOLD ENTERED LP-REFNO
      *  071196 MJD CHANGED PDTH DATES TO 9(6).
      *  990902 JTG ALLOWED NEGITIVE APPLIES AND BALANCES FOR OTH, OT2, LC, INT
      **************************************************************************
       01  RVENT-ERRCD            PIC X.

      * NOTE: RSVENTX-FIELDS MUST (MAP) RSVENT-FIELDS:
       01  RVENT-FIELDS.
           03  RVENT-PAYDATE          PIC 9(8).
           03  RVENT-TRCD             PIC X(2).
           03  RVENT-TRAMT            PIC S9(6)V99   COMP.
           03  RVENT-APOTH            PIC S9(6)V99   COMP.
           03  RVENT-APOT2            PIC S9(6)V99   COMP.
           03  RVENT-APINT            PIC S9(6)V99   COMP.
           03  RVENT-APLC             PIC S9(6)V99   COMP.
           03  RVENT-PRIOR-PAYDATE    PIC 9(8).
           03  RVENT-PRIOR-PAYAMT     PIC 9(6)V99    COMP.
           03  RVENT-PDTH-DATE        PIC 9(8)       COMP.
           03  RVENT-LCPDTH-DATE      PIC 9(8)       COMP.
           03  RVENT-OTHBAL           PIC S9(6)V99   COMP.
           03  RVENT-OT2BAL           PIC S9(6)V99   COMP.
           03  RVENT-INTBAL           PIC S9(6)V99   COMP.
           03  RVENT-LCBAL            PIC S9(6)V99   COMP.
           03  RVENT-PRINBAL          PIC S9(7)V99   COMP.
           03  RVENT-LC-DATE          PIC 9(8).
           03  RVENT-LC-AMT           PIC S9(6)V99   COMP.
           03  RVENT-INTPAID          PIC S9(6)V99   COMP.

      ********************************************************
      *    REVERSE ENTRY AMOUNT WORKERS
      ********************************************************
       01  RVENTX-FIELDS.
           03  RVENTX-PAYDATE          PIC 9(8).
           03  RVENTX-TRCD             PIC X(2).
               88 RVENTX-DF-POSTING    VALUE "DF" "D2" "D3" "D4"
                                             "D5" "D6" "D7" "D8"
                                             "D9".
           03  FILLER                 REDEFINES RVENTX-TRCD.
               05  FILLER              PIC X.
               05  RVENTX-NO-DEF       PIC 9.
           03  RVENTX-TRAMT            PIC S9(6)V99   COMP.
           03  RVENTX-APOTH            PIC S9(6)V99   COMP.
           03  RVENTX-APOT2            PIC S9(6)V99   COMP.
           03  RVENTX-APINT            PIC S9(6)V99   COMP.
           03  RVENTX-APLC             PIC S9(6)V99   COMP.
           03  RVENTX-PRIOR-PAYDATE    PIC 9(8).
           03  RVENTX-PRIOR-PAYAMT     PIC 9(6)V99    COMP.
           03  RVENTX-PDTH-DATE        PIC 9(8)       COMP.
           03  RVENTX-LCPDTH-DATE      PIC 9(8)       COMP.
           03  RVENTX-OTHBAL           PIC S9(6)V99   COMP.
           03  RVENTX-OT2BAL           PIC S9(6)V99   COMP.
           03  RVENTX-INTBAL           PIC S9(6)V99   COMP.
           03  RVENTX-LCBAL            PIC S9(6)V99   COMP.
           03  RVENTX-PRINBAL          PIC S9(7)V99   COMP.
           03  RVENTX-LC-DATE          PIC 9(8).
           03  RVENTX-LC-AMT           PIC S9(6)V99   COMP.
           03  RVENTX-INTPAID          PIC S9(6)V99   COMP.

      ********************************************************
      *   REVERSE ENTRY SCREEN DISPLAY BUFFER
      ********************************************************
       01  RV-DISPLAY-BUF          VALUE SPACES.
           03  RV-OTHBAL           PIC ZZZZ9.99-   BLANK ZERO.
           03  RV-OT2BAL           PIC ZZZZ9.99-   BLANK ZERO.
           03  RV-INTBAL           PIC ZZZZ9.99-   BLANK ZERO.
           03  RV-LCBAL            PIC ZZZZ9.99-   BLANK ZERO.

       01  RV-DISPLAY-LIST.
           03  FILLER              PIC X(16) VALUE
           "150 160 170 180.".

       01  RVMAX               PIC 99             VALUE 19.
       01  RV-ONE              PIC S9             COMP.
       01  RV-ELE              PIC 9(2)           COMP.
       01  RVENT-HOLDER        PIC S9(7)V99       COMP.
       01  RVENT-ALP-AMT       PIC ZZZZZ9.99-.
       01  RVENT-ALP-PDTH-DATE PIC 9(8).
       01  RVENT-REFNO         PIC X(5).

      *    **********************************
      *    *   RV-ELEMENT SPECIFICATIONS
      *    **********************************
       01  RVSPEC-TABLE.
      * 1. TRANSACTION DATE
           03  FILLER         PIC X(8) VALUE "ENNN060 ".
      * 2. TRAN CODE
           03  FILLER         PIC X(8) VALUE "ENNA020 ".
      * 3. TRAN AMOUNT
           03  FILLER         PIC X(8) VALUE "ENNN052 ".
      * 4. APOTH AMOUNT
           03  FILLER         PIC X(8) VALUE "ENNS052 ".
      * 5. APOT2 AMOUNT
           03  FILLER         PIC X(8) VALUE "ENNS052 ".
      * 6. APINT AMOUNT
           03  FILLER         PIC X(8) VALUE "ENNS052 ".
      * 7. APLC AMOUNT
           03  FILLER         PIC X(8) VALUE "ENNS052 ".
      * 8. LAST PMT DATE
           03  FILLER         PIC X(8) VALUE "ENNN060 ".
      * 9. DUMMY
           03  FILLER         PIC X(8) VALUE "E  A010N".
      *10. LAST PMT AMOUNT
           03  FILLER         PIC X(8) VALUE "ENNN052 ".
      *11. PAID THRU
           03  FILLER         PIC X(8) VALUE "ENNN060 ".
      *12. LCPDTH
           03  FILLER         PIC X(8) VALUE "ENNN060 ".
      *13. LATE CHARGE GRACE DATE
           03  FILLER         PIC X(8) VALUE "ENNN060 ".
      *14. LATE CHARGE GRACE AMOUNT
           03  FILLER         PIC X(8) VALUE "ENNN052 ".
      *15. OTHER BALANCE
           03  FILLER         PIC X(8) VALUE "ENNS052 ".
      *16. OTHER 2 BALANCE
           03  FILLER         PIC X(8) VALUE "ENNS052 ".
      *17. INT BALANCE
           03  FILLER         PIC X(8) VALUE "ENNS052 ".
      *18. LATE CHARGE BALANCE
           03  FILLER         PIC X(8) VALUE "ENNS052 ".
      *19. PRINCIPLE BALANCE
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
       01  RVSPEC-TAB        REDEFINES RVSPEC-TABLE.
           03  RVSPEC-REC    OCCURS 19 TIMES.
               05  RVSPEC    PIC X(7).
               05  RVCHFG    PIC X.

       01  RV-LEGEND           PIC X(70).
       01  RV-LEGEND-TABLE-BUF.
      * ELE 1
           03  FILLER          PIC X(45) VALUE
           "F1-CLEAR, ENTER 'THE PAYMENT DATE', OF THE PA".
           03  FILLER          PIC X(25) VALUE
           "YMENT YOUR REVERSING:    ".
      * ELE 2
           03  FILLER          PIC X(45) VALUE
           "F1-CLEAR, ^, ENTER 'THE PAYMENT CODE', OF THE".
           03  FILLER          PIC X(25) VALUE
           " PAYMENT YOUR REVERSING: ".
      * ELE 3
           03  FILLER          PIC X(45) VALUE
           "F1-CLEAR, ^, ENTER 'THE PAYMENT AMOUNT', OF T".
           03  FILLER          PIC X(25) VALUE
           "HE PAYMENT YOUR REVERSING".
      * ELE 4
           03  FILLER          PIC X(45) VALUE
           "F1-CLEAR, ^, ENTER 'APPLIED TO OTH 1', FOR TH".
           03  FILLER          PIC X(25) VALUE
           "E PAYMENT YOUR REVERSING:".
      * ELE 5
           03  FILLER          PIC X(45) VALUE
           "F1-CLEAR, ^, ENTER 'APPLIED TO OTH 2', FOR TH".
           03  FILLER          PIC X(25) VALUE
           "E PAYMENT YOUR REVERSING:".
      * ELE 6
           03  FILLER          PIC X(45) VALUE
           "F1-CLEAR, ^, ENTER 'APPLIED TO APINT', FOR".
           03  FILLER          PIC X(25) VALUE
           " THE PAYMENT YOUR REVERSI".
      * ELE 7
           03  FILLER          PIC X(45) VALUE
           "F1-CLEAR, ^, ENTER 'APPLIED LATE CHG', FOR TH".
           03  FILLER          PIC X(25) VALUE
           "E PAYMENT YOUR REVERSING:".
      * ELE 8
           03  FILLER          PIC X(45) VALUE
           "F1, ^, ENTER THE PRIOR PAYMENT DATE, TO THE P".
           03  FILLER          PIC X(25) VALUE
           "AYMENT BEING REVERSED:   ".
      * ELE 9     DUMMY
           03  FILLER          PIC X(45) VALUE
           "F1, ^, ENTER WHERE THE ACCOUNT WILL BE CONTRA".
           03  FILLER          PIC X(25) VALUE
           "CTUALLY AFTER REVERSING: ".
      * ELE 10
           03  FILLER          PIC X(45) VALUE
           "F1, ^, ENTER THE AMOUNT OF THE PRIOR PAYMENT ".
           03  FILLER          PIC X(25) VALUE
           "                         ".
      * ELE 11
           03  FILLER          PIC X(45) VALUE
           "F1, ^, ENTER WHERE THE ACCOUNT WILL BE CONTRA".
           03  FILLER          PIC X(25) VALUE
           "CTUALLY AFTER REVERSING: ".
      * ELE 12
           03  FILLER          PIC X(45) VALUE
           "F1, ^, ENTER WHERE THE ACCOUNT WILL BE LATE C".
           03  FILLER          PIC X(25) VALUE
           "HG PAID THRU AFTER REVERS".
      * ELE 13
           03  FILLER          PIC X(45) VALUE
           "F1-, ^, IF APPLIABLE, ENTER MONTH & YEAR OF A".
           03  FILLER          PIC X(25) VALUE
           "LATE CHARGE GRACE PERIOD:".
      * ELE 14
           03  FILLER          PIC X(45) VALUE
           "F1-, ^, IF APPLIABLE, ENTER THE $AMOUNT NO L/".
           03  FILLER          PIC X(25) VALUE
           "C CAN BE ASSESSED ON:    ".
      * ELE 15
           03  FILLER          PIC X(45) VALUE
           "F1-, ^, ENTER THE LOAN'S OTHER 1 BALANCE AFTE".
           03  FILLER          PIC X(25) VALUE
           "R THE PAYMENT REVERSAL:  ".
      * ELE 16
           03  FILLER          PIC X(45) VALUE
           "F1-, ^, ENTER THE LOAN'S OTHER 2 BALANCE AFTE".
           03  FILLER          PIC X(25) VALUE
           "R THE PAYMENT REVERSAL:  ".
      * ELE 17
           03  FILLER          PIC X(45) VALUE
           "F1-, ^, ENTER THE LOAN'S INTEREST BALANCE AFT".
           03  FILLER          PIC X(25) VALUE
           "ER THE PAYMENT REVERSAL: ".
      * ELE 18
           03  FILLER          PIC X(45) VALUE
           "F1-, ^, ENTER THE LOAN'S LATE CHG BALANCE AFT".
           03  FILLER          PIC X(25) VALUE
           "ER THE PAYMENT REVERSAL: ".
      * ELE 19
           03  FILLER          PIC X(45) VALUE
           "F1-, ^, ENTER THE LOAN'S PRINCIPAL BALANCE AF".
           03  FILLER          PIC X(25) VALUE
           "TER THE PAYMENT REVERSAL ".
       01  RV-LEGEND-TABLE    REDEFINES RV-LEGEND-TABLE-BUF.
           03  RV-LEGEND-ELE   OCCURS 19
                               PIC X(70).
