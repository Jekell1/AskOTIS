      * COPYMEMBER: LIBLP/LPMENUW
      ******************************************************************
      *
      *    L O A N   S U B M E N U
      *
      * REV:
      *  KEC 112196 ADDED 'ADDONS (G)' AND 'RESCISSION (Q)' SELECTIONS
      *
      ******************************************************************
       01  MENU-FIELD.
           03  FILLER             PIC X(4).
           03  MENU-5             PIC 9(1).
           03  FILLER             PIC X(1).
       01  MENU-FIELDX REDEFINES MENU-FIELD.
           03  FILLER             PIC X(4).
           03  MENU-NO            PIC 9(2).
       01  MENU-RESUME            PIC X VALUE "N".
       01  MENU-FG                PIC 9(2).
       01  MENU-MAX               PIC 99 VALUE 14.

       01  MENU-PG-FM             PIC X(48)   VALUE
           "L0LB0BC0CF0FH0HI0IM0MT0TX0XW0WY0YN0NG0GQ0Q".

       01  MENU-PG-FMX REDEFINES MENU-PG-FM.
           03  MENU-PROG-FORM     PIC X(3) OCCURS 14.

       01  MENU-NEW-PROG-FORM.
           03  MENU-NEW-PROG     PIC X.
           03  FILLER            PIC XX.
