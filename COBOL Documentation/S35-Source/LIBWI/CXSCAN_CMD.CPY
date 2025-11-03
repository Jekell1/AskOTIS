      * COPYMEMBER: LIBWI/CXSCAN.CMD
       01  LEGEND-01                  PIC X(47)  VALUE
           " F7-Exit, Scan Type: 1-Check Type, 2-Payee name".
       01  LEGEND-02                  PIC X(35)  VALUE
           " F7-Exit, ^-Type, Enter Check Type:".
       01  LEGEND-03                  PIC X(35)  VALUE
           " F7-Exit, ^-Type, Enter Payee Name:".
       01  LEGEND-04                  PIC X(30)  VALUE
           " F1, Select Check Record(rtn):".
       01  LEGEND-05                  PIC X(5)   VALUE
           "Type:".
       01  TYPE-PROMPT.
           03  FILLER                 PIC X(39)  VALUE 
           "Ty  Payee                         Amoun".
           03  FILLER                 PIC X(38)  VALUE 
           "t   Refno  Reason                     ".
       01  PAYEE-PROMPT.
           03  FILLER                 PIC X(39)  VALUE
           "Payee                         Amount  T".
           03  FILLER                 PIC X(38)  VALUE
           "y   Refno  Reason                     ".
