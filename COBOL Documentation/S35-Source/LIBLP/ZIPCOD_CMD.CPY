      * COPYMEMBER: LIBLP/ZIPCOD.CMD
      *=================================================================
      *
      *    Z I P C O D - L E G E N D - B U F F E R S
      *
      *=================================================================
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-Reset, F3-ALL, F6-Scan Grou".
           03  FILLER PIC X(30) VALUE "p, F7-Exit, Enter Beginning Br".
           03  FILLER PIC X(20) VALUE "anch Number         ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter Ending Branch Number      ".
           03  FILLER PIC X(20) VALUE "                    ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEG-CLASS.
           03  FILLER PIC X(30) VALUE "F1-Reset, F3-ALL, F6-Scan Clas".
           03  FILLER PIC X(30) VALUE "s, F7-Exit, UP/DOWN, Enter Beg".
           03  FILLER PIC X(20) VALUE "inning Loan Class   ".

       01  LEGEND-END-CLASS.
           03  FILLER PIC X(30) VALUE "F1-Reset, F6-Scan Class, F7-Ex".
           03  FILLER PIC X(30) VALUE "it, UP/DOWN, Enter Ending Loan".
           03  FILLER PIC X(20) VALUE " Class              ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-OPEN-ISO.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter Open Accounts (Include/Ski".
           03  FILLER PIC X(20) VALUE "p/Only)             ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PAID-ISO.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter Paid Accounts (Include/Ski".
           03  FILLER PIC X(20) VALUE "p/Only)             ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PL-ISO.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter P & L Accounts (Include/Sk".
           03  FILLER PIC X(20) VALUE "ip/Only)            ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PL-TYPE.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter P & L Type (All/PL/P2 thru".
           03  FILLER PIC X(20) VALUE " P5)                ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PL-ACTIVE.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter P & L Activity (Active/Ina".
           03  FILLER PIC X(20) VALUE "ctive/Both)         ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PRINT-DETAIL.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter to Print Detail Analysis (".
           03  FILLER PIC X(20) VALUE "Yes/No)             ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PRINT-ZIPCOD.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter to Print Zipcode Totals (Y".
           03  FILLER PIC X(20) VALUE "es/No)              ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PRINT-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter to Print Branch Totals (Ye".
           03  FILLER PIC X(20) VALUE "s/No)               ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-PRINT-CORP.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter to Print Corporate Totals ".
           03  FILLER PIC X(20) VALUE "(Yes/No)            ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-ENTER-LOCAL-PRINT-FG.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter to Spool Printing to Local".
           03  FILLER PIC X(20) VALUE " Directory (Yes/No) ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-ACCEPT.
           03  FILLER PIC X(30) VALUE "F1-Reset, F7-Exit, UP/DOWN, En".
           03  FILLER PIC X(30) VALUE "ter to Accept (Yes/No)        ".
           03  FILLER PIC X(20) VALUE "                    ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND-BEGIN-PROCESSING.
           03  FILLER PIC X(30) VALUE "Zipcode Inquiry Report in prog".
           03  FILLER PIC X(30) VALUE "ress..........................".
           03  FILLER PIC X(20) VALUE "....................".

       01  LEGEND-COMPILING.
           03  FILLER PIC X(08) VALUE "Branch: ".
           03  LEGEND-C-BRANCH  PIC 9(04) VALUE ZEROES.
           03  FILLER PIC X(18) VALUE " .................".
           03  FILLER PIC X(30) VALUE "Compiling Information.........".
           03  FILLER PIC X(20) VALUE "....................".

       01  LEGEND-PRINTING.
           03  FILLER PIC X(08) VALUE "Branch: ".
           03  LEGEND-P-BRANCH  PIC 9(04) VALUE ZEROES.
           03  FILLER PIC X(18) VALUE " .................".
           03  FILLER PIC X(30) VALUE "Printing Information..........".
           03  FILLER PIC X(20) VALUE "....................".

      *=================================================================
      *
      *    Z I P C O D - M E S S A G E - B U F F E R S
      *
      *=================================================================
       01  MESSAGE-INVALID-OPEN-ISO.
           03  FILLER PIC X(30) VALUE "INVALID OPEN ACCOUNT SELECTION".
           03  FILLER PIC X(30) VALUE ": (I)NCLUDE, (S)KIP, (O)NLY   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-PAID-ISO.
           03  FILLER PIC X(30) VALUE "INVALID PAID ACCOUNT SELECTION".
           03  FILLER PIC X(30) VALUE ": (I)NCLUDE, (S)KIP, (O)NLY   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-PL-ISO.
           03  FILLER PIC X(30) VALUE "INVALID PL ACCOUNT SELECTION: ".
           03  FILLER PIC X(30) VALUE "(I)NCLUDE, (S)KIP, (O)NLY     ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-PL-TYPE.
           03  FILLER PIC X(30) VALUE "INVALID PL TYPE SELECTION: (A)".
           03  FILLER PIC X(30) VALUE "LL, (P)L, P(2 THRU 5)         ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-PL-ACTIVE.
           03  FILLER PIC X(30) VALUE "INVALID PL ACTIVITY SELECTION:".
           03  FILLER PIC X(30) VALUE " (I)NACTIVE, (A)CTIVE, (B)OTH ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-PRINT-DETAIL.
           03  FILLER PIC X(30) VALUE "INVALID PRINT DETAIL SELECTION".
           03  FILLER PIC X(30) VALUE ": (Y)ES OR (N)O ONLY          ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-PRINT-ZIPCOD.
           03  FILLER PIC X(30) VALUE "INVALID PRINT ZIPCODE TOTALS S".
           03  FILLER PIC X(30) VALUE "ELECTION: (Y)ES OR (N)O ONLY  ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-PRINT-BRANCH.
           03  FILLER PIC X(30) VALUE "INVALID PRINT BRANCH TOTALS SE".
           03  FILLER PIC X(30) VALUE "LECTION: (Y)ES OR (N)O ONLY   ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-PRINT-CORP.
           03  FILLER PIC X(30) VALUE "INVALID PRINT CORPORATE TOTALS".
           03  FILLER PIC X(30) VALUE "SELECTION: (Y)ES OR (N)O ONLY ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-CORP-LOCAL.
           03  FILLER PIC X(30) VALUE "INVALID: CANNOT HAVE CORPORATE".
           03  FILLER PIC X(30) VALUE "TOTALS WITH LOCAL SPOOLING    ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MESSAGE-INVALID-NO-PRINTING.
           03  FILLER PIC X(30) VALUE "INVALID: MUST HAVE AT LEAST ON".
           03  FILLER PIC X(30) VALUE "E PRINT SELECTION TO 'YES'    ".
