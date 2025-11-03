      * COPYMEMBER: LIBWI/BWLNSC.CMD
       01  LEGEND-01                  PIC X(40)  VALUE
           "Scan Type: 1-Borr ssno, 2-Borrower name".
       01  LEGEND-02                  PIC X(40)  VALUE
           "F7-Exit, ^-Type, Enter Soc-Sec-No:".
       01  LEGEND-03                  PIC X(40)  VALUE
           "F7-Exit, ^-Type, Enter Lastname,First:".
       01  LEGEND-04                  PIC X(50)  VALUE
           "F1, F6-More Info, F4-All, Select Borrower(rtn):".
       01  LEGEND-05                  PIC X(5)   VALUE
           "Type:".
       01  SSNO-PROMPT.
           03  FILLER                 PIC X(39)  VALUE 
           "Ssno         Name                   Add".
           03  FILLER                 PIC X(18)  VALUE 
           "ress           #Ln".
       01  NAME-PROMPT.
           03  FILLER                 PIC X(39)  VALUE
           "Name                   Address         ".
           03  FILLER                 PIC X(18)  VALUE
           "   Ssno        #Ln".
