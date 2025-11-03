      * COPYMEMBER: LIBLP/PURFLDW
      *************************************************************************
      *                      LIBLP/PURFLDW
      *
      *    WORKER FOR COMPUTE-PURCHASE-FIELDS
      *    IN "LIBLP/PURFLDPD" AND "LIBLP/PURFLDLN".
      *    CALCULATIONS ARE PERFORMED ON WORKERS INSTEAD OF LN/PD
      *    FIELDS.
      *
      *  CHANGED IN PURFLDPD & PURFLDLN.
      * REV:
      *  JTG 032896 ADDED "-FG" FIELDS AND CHANGED FIELDS FROM COMP3 TO COMP
      *  MJD 990817 ADDED PURFLD-STAT.
      *  JTG 030617 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *             RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *             AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      **************************************************************************
       01  PURFLD-DLRVPRIN         OCCURS 3
                                   PIC 9(6)V99   COMP.
       01  PURFLD-DLRVCHGS         OCCURS 3
                                   PIC 9(6)V99   COMP.
       01  PURFLD-DLPARVHELD       PIC 9(6)V99   COMP.
       01  PURFLD-DLPARVPAID       PIC 9(6)V99   COMP.
       01  PURFLD-DLDISC           PIC 9(6)V99   COMP.

       01  PURFLD-DLRVPRIN-FG      OCCURS 3
                                   PIC X.
       01  PURFLD-DLRVCHGS-FG      OCCURS 3
                                   PIC X.
       01  PURFLD-DLPARVHELD-FG    PIC X.
       01  PURFLD-DLPARVPAID-FG    PIC X.
       01  PURFLD-DLDISC-FG        PIC X.
       01  PURFLD-STAT             PIC X(02).

       01  PURFLD-BUYRATE          PIC 9(3)V9(3) COMP-6  VALUE 0.
