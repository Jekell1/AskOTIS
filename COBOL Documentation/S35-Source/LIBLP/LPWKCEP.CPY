      * COPYMEMBER: LIBLP/LPWKCEP
      **************************************************************************
      *                           LPWKCEP
      *
      *  WHENEVER THE CALCULATION PROGRAM IS CALLED AND THE FIELD
      *  NAMED CIP-NO-OF-TIMES-CALLED IS SET TO ZERO THE FIELDS IN
      *  THIS AREA ARE FILLED IN. THESE NUMBERS THEN BECOME THE NEW
      *  UPPER EDIT LIMITS FOR THE CALLING PROGRAM.
      *
      *REV:
      * JTG 041394 ADDED CEP-FEE-CHG (FL) (BUY HERE PAY HERE)
      * JTG 031496 ADDED SIGNS TO FIELDS AND CHANGED FROM COMP-3 TO COMP
      * JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      * JTG 000112 ADDED CEP-INS-REJECT-TERM RE: SP-INSREJECT-FG OPTION 'C',
      *            WHICH WILL CAUSE WRITTING NO. OF MONTHS INSURANCE NOT IN
      *            VIOLATION, WISCONSIN #1032
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK PR#J021
      *
      * KEC 2015.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      *         CEPIX DEFINED IN LIBLP/LPWKINDEX.CPY
      *
      **************************************************************************

       01  CEP-CALCULATION-EDIT-AREA.
           10  CEP-INTERMED-FIELDS.
             15  CEP-RATES.
               20  CEP-EFF-RATE                PIC 9999V9999 COMP
                                                       VALUE ZERO.
               20  CEP-APR-RATE                PIC 9999V9999 COMP
                                                       VALUE ZERO.
             15  CEP-INSURANCES                OCCURS 8.
      *KEC 2015.0604-------------------------> INDEXED BY CEPIX.
               20  CEP-INS-TERM                PIC S9(4)    COMP.
               20  CEP-INS-COVERAGE            PIC S9(7)V99 COMP.
               20  CEP-INS-PREMIUM             PIC S9(7)V99 COMP.
               20  CEP-INS-REJECT-TERM         PIC S9(4)    COMP.
           10  CEP-OUTPUT-FIELDS.
             15  CEP-FEE-CHG                   OCCURS 2
                                               PIC S9(7)V99 COMP.
             15  CEP-SERVICE-CHG               PIC S9(7)V99 COMP.
             15  CEP-MAINT-FEES                PIC S9(7)V99 COMP.
             15  CEP-1ST-PY-EXT                PIC S9(7)V99 COMP.
             15  CEP-REGPYAMT                  PIC S9(7)V99 COMP.
      *-----------------------------------------------------------------
