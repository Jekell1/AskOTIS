      * COPYMEMBER: LIBUT/UT01ZM.CPY
      **************************************************
      *               STATE CODE FILE   
      * CREATED BY:MARK BOOMGAARD   06/2023  TRAINING
      * REV:  2023-06-23 MB CHANGE FOR SQL SERVER
      *           REMOVED FILLER
      **************************************************
       78  ZMB-SIZE                   VALUE 76.
       01  ZMB-REC.
           03  ZMB1-KEY.
               05  ZMB-CODE            PIC XX.
           03  ZMB-DESC                PIC X(16).
           03  ZMB-REINSTATE-WINDOW    PIC X.
           03  ZMB-REPO-RENOTICE       PIC X.
           03  ZMB-TITLE-RELEASE-TYPE  PIC X.
           03  ZMB-AUCTION-DATE-REQ    PIC X.
           03  ZMB-GEOGRAPHIC-CODE     PIC 9(04).
           03  ZMB-MAX-LOAN-AVAIL      PIC 9(6)V99  COMP-3.
           03  ZMB-MAX-TAX-LOAN-AMT    PIC S9(7)V99 COMP-3.
           03  ZMB-NSF-FEE-AMT         PIC S9(5)V99 COMP-3.
           03  ZMB-TEST-AMOUNT         PIC S9(7)V99.
           03  ZMB-TEST-CODE           PIC X(2).
           03  ZMB-TEST-DATE           PIC 9(6).
           03  ZMB-TEST-NUM            PIC 9(4).
           03  ZMB-TEST-RATE           PIC 9(3)V999.
           03  ZMB-TEST-SSNO           PIC 9(9).


