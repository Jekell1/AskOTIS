      * COPYMEMBER: LIBLP/LPREGZW
      *****************************************************************
      *                    (LPREGZW)
      *         ACTUARIAL / U.S. RULE UNEARNED ROUTINE WORKERS
      * REV:
      *******************************************************************
      * INPUT FIELDS:
       01  REGZ-DATE             PIC 9(8)        COMP.
       01  REGZ-LOANDATE         PIC 9(8)        COMP.
       01  REGZ-INTDATE          PIC 9(8)        COMP.
       01  REGZ-1STPYDATE        PIC 9(8)        COMP.
       01  REGZ-ORGTERM          PIC S9(5)       COMP.
       01  REGZ-1STPYAMT         PIC 9(6)V99     COMP.
       01  REGZ-REGPYAMT         PIC 9(6)V99     COMP.
       01  REGZ-LASTPYAMT        PIC 9(6)V99     COMP.
       01  REGZ-FINCHG           PIC S9(6)V99    COMP.
       01  REGZ-APRATE           PIC 9(3)V9(3)   COMP.
       01  REGZ-DISFRMLA         PIC X           VALUE "A".
       01  REGZ-UNITPER-CD       PIC X           VALUE "M".
       01  REGZ-UNITPER-FREQ     PIC 9(3)        VALUE 1.
       01  REGZ-INCLUDE-DEF-FG   PIC X           VALUE " ".
       01  REGZ-TOTNODEF         PIC 9(3)        COMP.

      * OUTPUT WORKERS:
       01  REGZ-UNEARNED         PIC S9(7)V99    COMP.
       01  REGZ-PRES-VALUE       PIC S9(7)V99    COMP.

      * INTERNAL WORKERS:
       01  REGZF                 PIC S9V9(15)    COMP.
       01  REGZT                 PIC S999        COMP.
       01  REGZ1P                PIC S9(6)V99    COMP.
       01  REGZP                 PIC S9(6)V99    COMP.
       01  REGZP-N               PIC S999        COMP.
       01  REGZP-FAC             PIC S999V9(15)  COMP.
       01  REGZBP                PIC S9(6)V99    COMP.
       01  REGZBP-N              PIC S999        COMP.
       01  REGZBP-FAC            PIC S999V9(15)  COMP.
       01  REGZI                 PIC S999V9(15)  COMP.
       01  REGZADJ               PIC S999V9(15)  COMP.
       01  REGZPW                PIC S999V9(15)  COMP.
       01  REGZPV                PIC S9(7)V99    COMP.
