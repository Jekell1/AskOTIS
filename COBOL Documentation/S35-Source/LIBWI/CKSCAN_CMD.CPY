      * COPYMEMBER: LIBWI/CKSCAN.CMD
       01  LEGEND-01                  PIC X(47)  VALUE
           " F7-Exit, F3-ALL, Enter Check Type:".
      *    " F7-Exit, Scan Type: 1-Check Type, 2-Payee name".
       01  LEGEND-02                  PIC X(35)  VALUE
           " F7-Exit, ^-Type, Enter Branch No. ".
       01  LEGEND-03                  PIC X(35)  VALUE
           " F7-Exit, ^-Type, Enter Branch No. ".
      *    " F7-Exit, Enter Branch No.         ".
       01  LEGEND-03B                 PIC X(35)  VALUE
           " F7-Exit, Enter Check No. Range:   ".
       01  LEGEND-03C                 PIC X(35)  VALUE
           " F7-Exit, Enter Check Date Range:  ".
       01  LEGEND-04                  PIC X(35)  VALUE
           " F1-Branch, Select Check (rtn):    ".
       01  LEGEND-04S                 PIC X(35)  VALUE
           " F7-Exit, F1, ^,v-Scan Checks:     ".
       01  LEGEND-05                  PIC X(5)   VALUE
           "Type:".

       01  TYPE-PROMPT.
           03  FILLER                 PIC X(25)  VALUE 
           " Check# Typ  Date   Payee".
           03  FILLER                 PIC X(17) VALUE SPACES.
           03  FILLER                 PIC X(20)  VALUE 
           "Amount Refno  Reason".
           03  FILLER                 PIC X(15) VALUE SPACES.

       01  PAYEE-PROMPT.
           03  FILLER                 PIC X(39)  VALUE
           "Payee                         Amount  T".
           03  FILLER                 PIC X(38)  VALUE
           "y   Refno  Reason                     ".
