      * COPYMEMBER: LIBNP/NP01NC
      ******************************************************************
      *
      *       NC-FILE - NOTE CLASS FILE
      * REV:
      *  MJD 110298 ADDED NC-ADD-INV-ALLOWED AND NC-EARLY-PENALTY-FG
      *  MJD 111198 ADDED NC-INTPY-DATE-CD
      *  MJD 111298 EXPANDED RECORD SIZE TO 150 BYTES.
      *             ADDED FORM REQUEST TABLE
      *  MJD 121098 ADDED NC-YRLYPMTS.
      *  MJD 121898 ADDED NC-SEQNO TO KEY.
      *  MJD 990106 MADE 2 A VALID YEARLY PAYMENT TYPE
      *  MJD 990122 ----- IMPORTED INTO FLEXGEN -----
      *  GLF 990219 ADD NC-VAR-RATE                      
      *  BLV 991011 CHANGE NC-FORMULA & NC-JEFF-CA TO UNUSED FIELDS
      *  BLF 080227 ADD NC-GRACE-DAYS, REGWAN PR# 3159
      *  AJM 121203 ADD NC-DORMANT-MONTHS, PR# 4150   
      *  BLM 130114 ADD NC-PENALTY-FG-2, 2 DIGIT TO ALLOW EARLY 
      *             PENALTIES WITH MORE THAN 9 MONTHS, WILL BE
      *             I-AM-REGENCY ONLY, REGWAN PR# 4186
      ******************************************************************
       78  NC-SIZE                  VALUE 150.
       01  NC-REC.
           03  NC1-KEY.
               05  NC-CLASS         PIC 99.
               05  NC-SEQNO         PIC 99.
           03  NC-DESC              PIC X(16).
           03  NC-TERMTYPE          PIC X.
           03  NC-MINTERM           PIC 9(4)     COMP-3.
           03  NC-MAXTERM           PIC 9(4)     COMP-3.
           03  NC-RATE              PIC 99V99    COMP-3.
           03  NC-INTMETHOD         PIC 9(4)     COMP-3.
           03  NC-MINAMT            PIC 9(6)     COMP-3.
           03  NC-EFFDATE           PIC 9(6)     COMP-3.
           03  NC-EXPDATE           PIC 9(6)     COMP-3.
           03  NC-UNUSED1           PIC X.
           03  NC-YEARTYPE          PIC 9(3).
           03  NC-UNUSED2           PIC X.
      *  ADDITIONAL INVESTMENTS ALLOWED?
      *  ARE INVESTORS ALLOWED TO MAKE ADDITIONAL INVESTMENTS ON A NOTE.
      *    EXAMPLE: ON A CD YOU CAN NOT INCREASE INVEST ANY MORE INTO
      *    THE NOTE THAN WAS INITIALLY PUT IN.
           03  NC-ADD-INV-ALLOWED   PIC X.
      *  EARLY PENALTY FLAG.
      *    CONTAINS THE NUMBER OF MONTHS PANALTY TO ENFORCE ON AN
      *    EARLY WITHDRAWL
           03  NC-EARLY-PENALTY-FG  PIC 9.
           03  NC-INTPAY-CDS         PIC X(4).
               88 NC-INTPYCD-F      VALUE "F   " " F  " "  F " "   F".
               88 NC-INTPYCD-N      VALUE "N   " " N  " "  N " "   N".
               88 NC-INTPYCD-P      VALUE "P   " " P  " "  P " "   P".
               88 NC-INTPYCD-T      VALUE "T   " " T  " "  T " "   T".
           03  NC-INTPYCD-TBL2 REDEFINES NC-INTPAY-CDS.
               05  NC-INTPYCD1       PIC X.
                   88 NC-INTPYCD1-F           VALUE "F".
                   88 NC-INTPYCD1-N           VALUE "N".
                   88 NC-INTPYCD1-P           VALUE "P".
                   88 NC-INTPYCD1-T           VALUE "T".
                   88 NC-INTPYCD1-VALID       VALUE "F" "N" "P" "T" " ".
               05  NC-INTPYCD2       PIC X.
                   88 NC-INTPYCD2-F           VALUE "F".
                   88 NC-INTPYCD2-N           VALUE "N".
                   88 NC-INTPYCD2-P           VALUE "P".
                   88 NC-INTPYCD2-T           VALUE "T".
                   88 NC-INTPYCD2-VALID       VALUE "F" "N" "P" "T" " ".
               05  NC-INTPYCD3       PIC X.
                   88 NC-INTPYCD3-F           VALUE "F".
                   88 NC-INTPYCD3-N           VALUE "N".
                   88 NC-INTPYCD3-P           VALUE "P".
                   88 NC-INTPYCD3-T           VALUE "T".
                   88 NC-INTPYCD3-VALID       VALUE "F" "N" "P" "T" " ".
               05  NC-INTPYCD4       PIC X.
                   88 NC-INTPYCD4-F           VALUE "F".
                   88 NC-INTPYCD4-N           VALUE "N".
                   88 NC-INTPYCD4-P           VALUE "P".
                   88 NC-INTPYCD4-T           VALUE "T".
                   88 NC-INTPYCD4-VALID       VALUE "F" "N" "P" "T" " ".
           03  NC-INTPYCD-TBL REDEFINES NC-INTPAY-CDS.
               05  NC-INTPYCD       PIC X   OCCURS 4.
               
           03  NC-INTPY-DATE-CD     PIC X.
      *----------------------------------------------------
      *    FORMS REQUEST TABLE
      *
      *    NOTE: SUB 1 IS RESERVED FOR CHECKS
      *    NOTE: SUB 2 IS RESERVED FOR RECEIPTS
      *----------------------------------------------------
           03  NC-REQ-FORM-TABLE             OCCURS 5.
               05  NC-PRFORM                 PIC 9(4)    COMP.
               05  NC-PRFLAG                 PIC X.
           03  NC-IMED-REC                   PIC X.
               88  NC-IMED-REC-VALID         VALUE "N" "Y".
           03  NC-YRLYPMTS-CDS         PIC 9(10).
               88 NC-YRLYPMTS-1                VALUE 1.
               88 NC-YRLYPMTS-3                VALUE 3.
               88 NC-YRLYPMTS-4                VALUE 4.
               88 NC-YRLYPMTS-6                VALUE 6.
               88 NC-YRLYPMTS-12               VALUE 12.
           03  NC-YRLYPMTS-TBL2 REDEFINES NC-YRLYPMTS-CDS.
               05  NC-YRLYPMTS1       PIC 99.
                   88 NC-YRLYPMTS1-1           VALUE 1.
                   88 NC-YRLYPMTS1-2           VALUE 2.
                   88 NC-YRLYPMTS1-3           VALUE 3.
                   88 NC-YRLYPMTS1-4           VALUE 4.
                   88 NC-YRLYPMTS1-6           VALUE 6.
                   88 NC-YRLYPMTS1-12          VALUE 12.
                   88 NC-YRLYPMTS1-VALID       VALUE 1 2 3 4 6 12.
               05  NC-YRLYPMTS2       PIC 99.
                   88 NC-YRLYPMTS2-1           VALUE 1.
                   88 NC-YRLYPMTS2-2           VALUE 2.
                   88 NC-YRLYPMTS2-3           VALUE 3.
                   88 NC-YRLYPMTS2-4           VALUE 4.
                   88 NC-YRLYPMTS2-6           VALUE 6.
                   88 NC-YRLYPMTS2-12          VALUE 12.
                   88 NC-YRLYPMTS2-VALID       VALUE 0 1 2 3 4 6 12.
               05  NC-YRLYPMTS3       PIC 99.
                   88 NC-YRLYPMTS3-1           VALUE 1.
                   88 NC-YRLYPMTS3-2           VALUE 2.
                   88 NC-YRLYPMTS3-3           VALUE 3.
                   88 NC-YRLYPMTS3-4           VALUE 4.
                   88 NC-YRLYPMTS3-6           VALUE 6.
                   88 NC-YRLYPMTS3-12          VALUE 12.
                   88 NC-YRLYPMTS3-VALID       VALUE 0 1 2 3 4 6 12.
               05  NC-YRLYPMTS4       PIC 99.
                   88 NC-YRLYPMTS4-1           VALUE 1.
                   88 NC-YRLYPMTS4-2           VALUE 2.
                   88 NC-YRLYPMTS4-3           VALUE 3.
                   88 NC-YRLYPMTS4-4           VALUE 4.
                   88 NC-YRLYPMTS4-6           VALUE 6.
                   88 NC-YRLYPMTS4-12          VALUE 12.
                   88 NC-YRLYPMTS4-VALID       VALUE 0 1 2 3 4 6 12.
               05  NC-YRLYPMTS5       PIC 99.
                   88 NC-YRLYPMTS5-1           VALUE 1.
                   88 NC-YRLYPMTS5-2           VALUE 2.
                   88 NC-YRLYPMTS5-3           VALUE 3.
                   88 NC-YRLYPMTS5-4           VALUE 4.
                   88 NC-YRLYPMTS5-6           VALUE 6.
                   88 NC-YRLYPMTS5-12          VALUE 12.
                   88 NC-YRLYPMTS5-VALID       VALUE 0 1 2 3 4 6 12.
           03  NC-YRLYPMTS-TBL REDEFINES NC-YRLYPMTS-CDS.
               05  NC-YRLYPMTS       PIC 99   OCCURS 5.
           03  NC-VAR-RATE           PIC X.                
           03  NC-GRACE-DAYS         PIC 9(4) COMP-6.
           03  NC-DORMANT-MONTHS     PIC 9(4) COMP-6.
           03  NC-PENALTY-FG-2       PIC 99   COMP-6.
           03  FILLER                        PIC X(61).
