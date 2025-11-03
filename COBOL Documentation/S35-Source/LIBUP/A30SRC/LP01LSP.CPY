      * COPYMEMBER: LIBLP/LP01LSP
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *
      *       PAID THRU (DEFERMENT) MODIFICATION APPROVAL
      *
      * REV:
      *  BAH 050914 ADDED "P" RECORD TYPE FOR PAID THRU MODIFICATION APPROVAL
      *             WINDOW, LENDMARK PR# 2568
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ******************************************************************
       78  LSP-SIZE                       VALUE 650.
       01  LSP-REC.
           03  LSP1-KEY.
               05  LSP-BRNO               PIC 9(4).
               05  LSP-ACCTNO             PIC 9(8).
               05  LSP-SEQNO              PIC 99.

      *>>XDD DATE=YYYYMMDD
           03  LSP-LTOUCH-DATE            PIC 9(8)     COMP-3.

           03  LS-PAID-THRU-AREA.
               05  LSP-REASON-CODE           PIC XX.
               05  LSP-PERIODS-TO-DEFER      PIC 99.
               05  LSP-REASON-FOR-DEFER      PIC X(50).
               05  LSP-MORTGAGE-PAST-DUE     PIC X.
               05  LSP-MORTGAGE-BALANCE      PIC S9(7)V99 COMP-3.
               05  LSP-ATTEMPT-REFINANCE     PIC X(50).
      *>>XDD DATE=YYYYMMDD
               05  LSP-INFO-DATE             PIC 9(8) COMP-3.
               05  LSP-INFO-USERID           PIC X(10).
               05  LSP-BR-MANAGER-COMMENTS   PIC X(50).
      *>>XDD DATE=YYYYMMDD
               05  LSP-BR-MANAGER-DATE       PIC 9(8) COMP-3.
               05  LSP-BR-MANAGER-USERID     PIC X(10).
               05  LSP-AVP-COMMENTS          PIC X(50).
      *>>XDD DATE=YYYYMMDD
               05  LSP-AVP-DATE              PIC 9(8) COMP-3.
               05  LSP-AVP-USERID            PIC X(10).
               05  LSP-AVP-APPROVE           PIC X.
               05  LSP-VP-COMMENTS           PIC X(50).
      *>>XDD DATE=YYYYMMDD
               05  LSP-VP-DATE               PIC 9(8) COMP-3.
               05  LSP-VP-USERID             PIC X(10).
               05  LSP-VP-APPROVE            PIC X.

               05  FILLER                     PIC X(309).

