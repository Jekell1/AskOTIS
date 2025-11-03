      * COPYMEMBER: LIBNP/NP01IV
      **************************************************
      *       IV-FILE - INVESTOR FILE
      *
      * ****** IF CHANGING THIS FILE ALSO CHAGE LIBNP/NPCHIV  ******
      *
      * REV:
      *  JTG 930816 ADDED IV-SEND-CK-FG (AUTO CREDIT MI)
      *  MJD 990122 ----- IMOPORTED INTO FLEXGEN -----
      *  CS  1999.0309 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  MJD 991221 CHANGED INTWHELD TO BR INTWHELD-YTD
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  MJD 061121 ADDED IV-POSTION AND IV-REFDATE. REGENCY PR#2856.
      *   CS 160307 ADDED IV-CITIZEN     REGENCY PR#5002
      *
      **************************************************
       78  IV-SIZE                     VALUE 380.
       01  IV-REC.
           03  IV1-KEY.
               05  IV-SSNO             PIC 9(10)     COMP-3.
           03  IV-NAMES.
               05  IV-LNAME            PIC X(16).
               05  IV-FNAME            PIC X(16).
           03  IV-NAMESX REDEFINES IV-NAMES.
               05  IV2-KEY             PIC X(17).
               05  FILLER              PIC X(15).
           03  IV-ADDRTBL              OCCURS 2.
               05  IV-ADR1             PIC X(24).
               05  IV-ADR2             PIC X(24).
               05  IV-CITY             PIC X(15).
               05  IV-STATE            PIC XX.
               05  IV-ZIP              PIC X(10).
               05  IV-PHONE            PIC 9(10)     COMP-3.
               05  IV-SALUTE           PIC X(30).
           03  IV-CLASS                PIC XX.
           03  IV-BIRTH                PIC 9(6)      COMP-3.
           03  IV-EXEMPA               PIC 9(2)      COMP-3.
           03  IV-EXEMPB               PIC X(6).
           03  IV-INTLYR               PIC S9(7)V99  COMP-3.
           03  IV-INTYTD               PIC S9(7)V99  COMP-3.
           03  IV-INTWHELD-YTD         PIC S9(7)V99  COMP-3.
           03  IV-NEWNOTES             PIC 9(4)      COMP-3.
           03  IV-RENNOTES             PIC 9(4)      COMP-3.
           03  IV-RESPONSE             PIC 9(2)      COMP-3.
           03  IV-SEND-CK-FG           PIC X.

           03  IV-KEYTYPE              PIC X.
               88  IV-KEYTYPE-PERS     VALUE "P".
               88  IV-KEYTYPE-ORG      VALUE "O".
               88  IV-KEYTYPE-BUS      VALUE "B".
               88  IV-KEYTYPE-VALID    VALUE "P" "O" "B".
               88  IV-KEYTYPE-TAX-ID   VALUE "O" "B".
           03  IV-POSITION             PIC X(24).
           03  IV-REFDATE              PIC 9(6) COMP.
           03  IV-CITIZEN              PIC X.
           03  FILLER                  PIC X(53).
