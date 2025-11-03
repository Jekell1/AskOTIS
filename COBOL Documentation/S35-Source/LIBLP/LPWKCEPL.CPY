      * COPYMEMBER: LIBLP/LPWKCEPL

      **************************************************************************
      *                      LPWKCEPL
      *
      *  WHENEVER THE CALCULATION PROGRAM IS CALLED AND THE FIELD
      *  NAMED CIP-NO-OF-TIMES-CALLED IS SET TO ZERO THE FIELDS IN
      *  THIS AREA ARE FILLED IN. THESE NUMBERS THEN BECOME THE NEW
      *  UPPER EDIT LIMITS FOR THE CALLING PROGRAM.
      *
      * REV:
      * JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      * JTG 000112 ADDED CEP-INS-REJECT-TERM RE: SP-INSREJECT-FG OPTION 'C',
      *            WHICH WILL CAUSE WRITTING NO. OF MONTHS INSURANCE NOT IN
      *            VIOLATION, WISCONSIN #1032
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK PR#J021
      *
      * KEC 2015.0604 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS SO TO 
      *         PROPERLY WORK FOR USE WITH MICROFOCUS.
      *         CEPIX DEFINED IN LIBLP/LPWKINDEX.CPY
      *
      **************************************************************************
       01  CEP-CALCULATION-EDIT-AREA.
           10  CEP-INTERMED-FIELDS.
             15  CEP-RATES.
               20  CEP-EFF-RATE     PIC 9999V9999 COMP-3 VALUE ZERO.
               20  CEP-APR-RATE     PIC 9999V9999 COMP-3 VALUE ZERO.
             15  CEP-INSURANCES                OCCURS 8.
      *KEC 2015.0604-------------------------> INDEXED BY CEPIX.
               20  CEP-INS-TERM                PIC 9999    COMP-3.
               20  CEP-INS-COVERAGE            PIC 9(7)V99 COMP-3.
               20  CEP-INS-PREMIUM             PIC 9(7)V99 COMP-3.
               20  CEP-INS-REJECT-TERM         PIC 9999    COMP-3.
           10  CEP-OUTPUT-FIELDS.
             15  CEP-SERVICE-CHG               PIC 9(7)V99 COMP-3.
             15  CEP-MAINT-FEES                PIC 9(7)V99 COMP-3.
             15  CEP-1ST-PY-EXT                PIC 9(7)V99 COMP-3.
             15  CEP-REG-PAYMENT               PIC 9(7)V99 COMP-3.
      *-----------------------------------------------------------------
