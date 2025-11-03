       01  EMPLR4-SIGNATURE-LINE1.
           03  FILLER                       PIC X(49) VALUE
               "By signing below, I certify that I have reviewed ".
           03  FILLER                       PIC X(48) VALUE
               "each customer file (external and system) listed ".
           03  FILLER                       PIC X(35) VALUE
               "above in accordance with Company".
       01  EMPLR4-SIGNATURE-LINE2.
           03  FILLER                       PIC X(51) VALUE
               "policy in the OPERATIONS MANUAL CHAPTER 11 section ".
           03  FILLER                       PIC X(51) VALUE
               "A:".
       01  EMPLR4-SIGNATURE-LINE3.
           03  FILLER                       PIC X(44) VALUE
               "DISTRICT SUPERVISOR: ___________________    ".
           03  FILLER                       PIC X(44) VALUE
               "________________________________________    ".
           03  FILLER                       PIC X(44) VALUE
               "________________________________________    ".
       01  EMPLR4-SIGNATURE-LINE4.
           03  FILLER                       PIC X(44) VALUE
               "                       (District code)      ".
           03  FILLER                       PIC X(44) VALUE
               "            (Printed Name)                  ".
           03  FILLER                       PIC X(44) VALUE
               "              (Signature)                   ".

       01  LEGEND-BR-01.
           03  FILLER           PIC X(49)  VALUE
               "F1-PRINTER, F7-EXIT, v, ENTER BEGINNING BRANCH NO".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-BR-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING BRANCH NO. :".

       01  LEGEND-CL-01.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING CLASS NO".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-CL-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING CLASS NO. : ".

       01  LEGEND-GTT-01.
           03  FILLER           PIC X(49)  VALUE
               " F7-EXIT, ^, v, ENTER (I)NCLUDE, (S)KIP, (O)NLY :".
       01  LEGEND-INCLDETAIL.
           03  FILLER           PIC X(38)  VALUE
               "^, v, INCLUDE DETAIL LINES ON REPORT?:".
       01  LEGEND-DTE.
           03  FILLER           PIC X(38)  VALUE
               "F7-EXIT, ^, v, ENTER MONTH ENDING DATE".
           03  FILLER           PIC X(8) VALUE
               " (MMYY):".
       01  LEGEND-DA-01.
           03  FILLER           PIC X(43)  VALUE
               "F7-EXIT, ^, v, ENTER DAY OF MONTH (F3-ALL):".
       01  LEGEND-LOCAL-SPOOL.
           03  filler           pic x(31)  value
               "SPOOL TO LOCAL DIRECTORY (Y/N):".
       01  LEGEND-ACCEPT.
           03  FILLER           PIC X(40)  VALUE
               "F7-EXIT, F1, ^, ACCEPT (Y/N):".
