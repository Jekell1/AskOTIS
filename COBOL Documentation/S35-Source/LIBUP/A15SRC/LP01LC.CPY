      * COPYMEMBER: LIBLP/LP01LC
      **********************************************************
      *         LOAN CLASS FILE
       78  LC-SIZE                    VALUE 44.
      *
      *---------------------------------------------------------
      *                       REVOLVING OPTIONS
      *
      *                 DIRECT LOAN          SALESFIN
      *                 -----------          --------
      *  SMALLOAN :        Y/N                  Y/N
      *  SALEFIN  :        N                    Y  
      *  SERVICE  :        N                    N
      *  REVOLVING:        Y/N                  Y/N
      *---------------------------------------------------------
      * REV:
      *  MJD 040896 ADDED LC-AFP-CLASS.
      *  JTG 071896 ADDED LC-CON-PDUE-DAYS  LC-REC-PDUE-DAYS
      *  JTG 081397 ADDED LC-GIFTNET WORLD A90
      *   CS 031098 ADDED LC-APP-REQ-FOR-LOAN
      *  JTG 082698 ADDED LC-REVOLVING FLAG, REGENCY
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  MJD 990722 ADDED 88'S TO AFP-CLASS
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  CS  010314 ADDED LC-POOLID MERC 
      *  BAH 030423 ADD LC-USE-ALT-GL-FEE, FOR LENDMARK PR# 2048
      *  BLV 030728 ADDED LC-PROGRP-CLASS, TO ALLOW GROUPING CERTAIN
      *             CLASSES TOGETHER IN TOP SECTION OF PROGRESS REPORT,
      *             EXCEL, PR# 2098
      *  BAH 071009 ADDED LC-ALLOW-CASH-PROCEEDS FOR REGMGM PR# 3074
      *  BAH 121001 ADDED LC-SALES-FORCED-ENTRY REDEFINES LC-STOP-ON-DLNO
      *             TO FORCE RESERVE/DISCOUNT ENTRIES FOR SEC AUTO PR# 4116
      *******************************************************************
       01  LC-REC.
           03  LC1-KEY.
               05  LC-CODE            PIC 99.
           03  LC-DESC                PIC X(16).
           03  LC-SMALLOAN            PIC X.
           03  LC-SALEFIN             PIC X.
           03  LC-SERVACCT            PIC X.
           03  LC-LOANTYPE            PIC X.
           03  LC-PROF.                        
               05  LC-PROF1           PIC X.
               05  LC-PROF2           PIC X.
           03  LC-SKIP-TRW            PIC X.
           03  LC-DISPLAY-ERN         PIC X.
           03  LC-LCARDFG             PIC X.
           03  LC-CALC-HI-CREDIT      PIC X.
           03  LC-STOP-ON-DLNO        PIC X.
           03  LC-SALES-FORCED-ENTRY REDEFINES LC-STOP-ON-DLNO PIC X.
           03  LC-AFP-CLASS           PIC X.
               88 LC-AFP-CLASS-N      VALUE " " "N".
               88 LC-AFP-CLASS-Y      VALUE "Y".
               88 LC-AFP-CLASS-VALID  VALUE "Y" "N".
           03  LC-PASTDUE.
               05  LC-CON-PDUE-DAYS   PIC X.
               05  LC-REC-PDUE-DAYS   PIC X.
           03  LC-GIFTNET             PIC X.
           03  LC-APP-REQ-FOR-LOAN    PIC X.
           03  LC-REVOLVING           PIC X.
           03  LC-POOLID              PIC 9(5) COMP-3.  
           03  LC-USE-ALT-GL-FEE      PIC X.
           03  LC-PROGRP-CLASS        PIC 99.
           03  LC-ALLOW-CASH-PROCEEDS PIC X.
           03  FILLER                 PIC X(2).
