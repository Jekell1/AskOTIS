      * COPYMEMBER: LIBLP/LP01NO
      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LP01NO
      *
      * DESCRIPTION   : NOTICE FILE [NOFILE] RECORD LAYOUT
      *
      * RECORD SIZE   : 176 BYTES
      *
      *=================================================================
      *
      * *** NOTE *** FLEXGEN *** NOTE *** FLEXGEN *** NOTE *** FLEXGEN
      *
      * CHANGES TO NO-REC WILL AUTOMATICALLY UPDATE TO THE 
      * FG-REC (LIBCL/CL01FG; FG-PFS-NO-REC) UNLESS YOU ARE CHANGING
      * THE SIZE OF NO-REC WHICH THEN YOU NEED TO CHANGE THE FG-REC.
      * YOU STILL NEED ANY CHANGES OF NO-REC NEED TO BE UPDATED WITHIN
      * FLEXGEN.....PLEASE NOTIFY THE PFS FLEXGEN ADMINISTRATOR.
      *
      * WITHIN FLEXGEN, THE NOTICE FILE IS GENEREATED BY CALLING TO
      * THE LOAN MANAGER (CL/FLEXGN).  IT WILL UPDATE THE NO-REC THEN
      * MOVES IT TO THE FG-REC (LIBCL/CL01FG; FG-PFS-NO-REC) WHICH IS
      * RETURNED BACK TO FLEXGEN.
      *
      *=================================================================
      * REV:
      * JTG 072897 ADDED NO-WAGE-ASSIGN-DELINQ  MERCURY #E135
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      * BAH 000726 ADDED NO-PAST-DUE-DAYS, PR# 1170 SUPPORT
      * KEC 170728 ADDED DOCUMENTATION ON THE USE OF FLEXGEN WITH
      *            THE NO-REC AND FG-REC (LIBCL/CL01FG).
      *
      ******************************************************************
       01  NO-REC.
           03  NO1-KEY.
               05  NO-ACCTNO       PIC 9(8).
               05  NO-FMNO         PIC 9(3).
           03  NO-BRNO             PIC 9(4)     COMP-3.
      *----TO DATE DUE FIGURES-----------------------
           03  NO-TDUE             PIC 9(6)     COMP-3.
           03  NO-TDUELC           PIC S9(7)V99 COMP-3.
           03  NO-TDUEAMT          PIC S9(7)V99 COMP-3.
           03  NO-TDUEPRIN         PIC S9(7)V99 COMP-3.
           03  NO-TDUETBAL         PIC S9(7)V99 COMP-3.
           03  NO-TDUEPOFF         PIC S9(7)V99 COMP-3.
      *----LAST DUE FIGURES--------------------------
           03  NO-LDUE             PIC 9(6)     COMP-3.
           03  NO-LDUELC           PIC S9(7)V99 COMP-3.
           03  NO-LDUEAMT          PIC S9(7)V99 COMP-3.
           03  NO-DATE-PAID-LAST   PIC 9(6)     COMP-3.
           03  NO-AMT-PAID-LAST    PIC S9(7)V99 COMP-3.
           03  NO-LSTLPKEY         PIC X(12).
      *----NEXT DUE FIGURES--------------------------
           03  NO-NXDUE            PIC 9(6)     COMP-3.
           03  NO-NXDUEAMT         PIC S9(7)V99 COMP-3.
           03  NO-NXDUELC          PIC S9(7)V99 COMP-3.
      *----MISCELLANEOUS-----------------------------
           03  NO-TRUCONTR         PIC 9(4)     COMP-3.
           03  NO-RECENCY          PIC 9(4)     COMP-3.
           03  NO-PDTH             PIC 9(6)     COMP-3.
           03  NO-LCPDTH           PIC 9(6)     COMP-3.
           03  NO-PRINTED          PIC X.
           03  NO-LCHG-INTDUE      PIC S9(7)V99 COMP-3.
           03  NO-AVAILCREDIT      PIC S9(7)V99 COMP-3.
           03  NO-TDUEDELINQ       PIC S9(7)V99 COMP-3.
           03  NO-DEFERMENT        PIC S9(7)V99 COMP-3.
           03  NO-TRUE-DUE-DATE    PIC 9(6)     COMP-3.
           03  NO-SCHED-PAYMENT    PIC S9(7)V99 COMP-3.

      *----TO DATE DUE CONTR-------------------------
           03  NO-TDUECONTR        PIC S9(7)V99 COMP-3.
      *----LAST DUE CONTR----------------------------
           03  NO-LDUECONTR        PIC S9(7)V99 COMP-3.
      *----NEXT DUE CONTR----------------------------
           03  NO-NXDUECONTR       PIC S9(7)V99 COMP-3.

           03  NO-CURRENT-AVAIL    PIC S9(7)V99 COMP-3.
           03  NO-LATE-CHG         PIC S9(7)V99 COMP-3.

           03  NO-WAGE-ASSIGN-DELINQ  PIC S9(7)V99 COMP-3.
           03  NO-PAST-DUE-DAYS    PIC 9(4)     COMP-3.
           03  FILLER              PIC X(7).
