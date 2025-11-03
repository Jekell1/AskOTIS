      * COPYMEMBER: LIBWI/GRTOTLW
      ******************************************************************
      *
      *    G R O U P - T O T A L S - T A B L E --- WINDOW WORKERS
      *
      * 2023.0605 ADDED GRTOTL-FILLER AS WI/GRTOTL MOVES SCAN-BUF (LINK-BUF)
      *           TO GRTOTL-BUF AND SCAN-TITLE WAS ADDED TO SCAN-BUF
      * 2024.0507 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  GRTOTL-BUF.
           03  GRTOTL-ENT-GROUP         PIC X(4).
           03  GRTOTL-SV-ENT-GROUP      PIC X(4).
           03  GRTOTL-TOT-LEV-TABLE.
               05  GRTOTL-TOT-LEV-TAB   PIC X(1)  OCCURS 10.
