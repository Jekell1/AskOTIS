      * COPYMEMBER: LIBSP/BHPURG.CMD
       01  LEGEND-USERID-01.
           03  FILLER           PIC X(44)  VALUE
               " F7-EXIT, v, ENTER BEGINNING USER ID".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-USERID-02.
           03  FILLER           PIC X(33)  VALUE
               "^, v, ENTER ENDING USER ID :".
       01  LEGEND-DATE-01.
           03  FILLER           PIC X(36)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING DATE".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-DATE-02.
           03  FILLER           PIC X(30)  VALUE
               "^, v, ENTER ENDING DATE : ".
       01  LEGEND-ACCEPT.
           03  FILLER           PIC X(40)  VALUE
               "F7-EXIT, F1, ^, ACCEPT (Y/N):".
       01  BABY-ZS              PIC X(10)  VALUE "zzzzzzzzzz".
