      * COPYMEMBER: LIBGL/GLSCAN.CMD
       01  LEGEND-01                  PIC X(40)  VALUE
           "Scan Type: 1=G/L Acct No., 2=Description".
       01  LEGEND-02                  PIC X(40)  VALUE
           "F7-Exit, ^-Type, Enter Branch No:".
       01  LEGEND-03A                 PIC X(40)  VALUE
           "F7-Exit, ^-Type, Enter 7 digit Acct No:".
       01  LEGEND-03B                 PIC X(40)  VALUE
           "F7-Exit, ^-Type, Enter G/L Description:".
       01  LEGEND-04                  PIC X(40)  VALUE
           "F1, F6-More Info, Select Account(rtn):".
       01  WGLACCT-PROMPT.
           03  FILLER                 PIC X(40)  VALUE 
           "Scan by BRANCH / ACCOUNT NO.            ".
       01  WGLDESC-PROMPT.
           03  FILLER                 PIC X(40)  VALUE 
           "Scan by G/L DESCRIPTION                 ".
       01  SELECT-PROMPT.
           03  FILLER                 PIC X(46)  VALUE 
           "PgUp, PgDn, <RTN>=Select, F1=Restart, F7=Exit".
