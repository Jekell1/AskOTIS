      * COPYMEMBER: LIBWI/LESCANW
      * REV:
      *  2024.0417 BAH REMOVED LESCAN-WIMENU-SELECTION,
      *                LESCAN-CLMENU-SELECTION
      *  2024.0425 BAH MADE LEVE 01 AND CALL, NOT WINDOW
       01  LESCAN-BUF.
           03  LESCAN-TYPE             PIC X        VALUE "R".
           03  LESCAN-BRNO             PIC 9(4).
           03  LESCAN-ACCTNO           PIC 9(8)     COMP.
           03  LESCAN-SSNO             PIC 9(9)     COMP OCCURS 4.
           03  LESCAN-MAKERCD          PIC X             OCCURS 4.
           03  LESCAN-LNAMT            PIC 9(6)V99  COMP.
