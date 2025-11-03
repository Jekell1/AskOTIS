      * COPYMEMBER: LIBWI/CJSCAN.CMD
       01  LEGEND-01                  PIC X(50)  VALUE
           " F7-Exit, Scan Type: 1-Detail, 2-Due, 3-Balances  ".
       01  LEGEND-02                  PIC X(50)  VALUE
           " Enter Branch No,  F7-Exit :                      ".
       01  LEGEND-03                  PIC X(50)  VALUE
           " ALL = All Collectors, F1-Brno, F7-Exit :         ".
       01  LEGEND-03B                 PIC X(50)  VALUE
           " F1-Brno, F7-Exit, ^-Collector, Enter Start Date: ".
       01  LEGEND-04                  PIC X(50)  VALUE
           " F1-Type,^-Up,v-Down, Select Detail (rtn):        ".
       01  LEGEND-05                  PIC X(5)   VALUE
           "Type:".
       01  DETAIL-PROMPT.
           03  FILLER                 PIC X(42)  VALUE 
           "Brno  Collid   Date   Start-End   Acctno  ".
           03  FILLER                 PIC X(35)  VALUE 
           "Actv Resl Action Contr Recy Promise".
       01  DUE-PROMPT.
           03  FILLER                 PIC X(40)  VALUE
           "Cd   Date     Acctno   Proceeds         ".
           03  FILLER                 PIC X(37)  VALUE
           "Paid        Due      -- Check Info --".
       01  BAL-PROMPT.
           03  FILLER                 PIC X(40)  VALUE
           "Acctno  Date  Proceeds    Paid/Rel      ".
           03  FILLER                 PIC X(37)  VALUE
           "  Due   Prin Rsv    Chg Rsv     Held ".
