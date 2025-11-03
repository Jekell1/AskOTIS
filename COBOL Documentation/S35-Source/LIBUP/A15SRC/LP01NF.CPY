      * COPYMEMBER: LIBLP/LP01NF
      *****************************************************************
      *                    (NF-FILE)
      *    CHECK/MO INDEX
      *    USED:  FOR LOCATING BRANCH NSF CHECK SHOULD BE RETURNED TO
      *
      *                      *NEW*  PR#2034 CASHCO
      * REV:
      * 030410  DON'T STORE THE ADDRESS PER JTG  
      *****************************************************************
       78  NF-SIZE                 VALUE  78.    
       01  NF-REC.
           03  NF1-KEY.
               05  NF-CK-NUMBER       PIC 9(10).
               05  NF2-KEY.
                   07  NF-LNAME       PIC X(16).
                   07  NF-FNAME       PIC X(16).
               05  NF-CK-SEQUENCE     PIC 9(4).

           03  NF-ACCTNO              PIC 9(8) COMP-6.
           03  FILLER REDEFINES NF-ACCTNO.
               05  NF-NUMBER          PIC 9(6) COMP-6.
               05  NF-SEQUENCE        PIC 9(2) COMP-6.
           03  NF-TRDATE              PIC 9(6) COMP-6.
           03  NF-OWNBR               PIC 9(4) COMP-6.
           03  NF-POSTING-BR          PIC 9(4) COMP-6.
           03  NF-REFNO               PIC X(5).
           03  NF-TRAMT               PIC S9(7)V99 COMP-3.
           03  NF-REV                 PIC X.
           03  FILLER                 PIC X(10).
