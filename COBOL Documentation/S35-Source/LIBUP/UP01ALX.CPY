      ******************************************************************
      *
      * ASCII LOAN PAYMENT TRAILER FILE: 
      *  TAB DELMITED
      *
      * CODE:
      *  A - ADDON TRAILER RECORD
      *  M - MONTHLY INSURANCE PREMIUM INFO
      *  Z - AMORTIZED INTEREST (Z) PAYMENT INFO
      *  T - TRANFER
      *  P - TRANFER (PURCHASED)
      *  X - INSURANCE TAX
      *  R - REBATE AMOUNT FOR GEORGIA 4% CLOSING FEE EARLY PROVISION
      *
      ******************************************************************
       78  ALX-SIZE                  VALUE 967. 
       01  ALX-REC.
           03  ALX-BRNO               PIC 9(4).
           03  FILLER                 PIC X.
           03  ALX-ACCTNO             PIC 9(8).
           03  FILLER                 PIC X.
           03  ALX-SEQNO              PIC 9(5).
           03  FILLER                 PIC X.
           03  ALX-LTOUCH-DATE        PIC 9(8).
           03  FILLER                 PIC X.

           03  ALXT-CODE              PIC X.
           03  FILLER                 PIC X.
           03  ALXT-TABLE         OCCURS 20.
               05  ALXT-EARN          PIC +9(7).99.
               05  FILLER             PIC X.

           03  ALXP-CODE              PIC X.
           03  FILLER                 PIC X.
           03  ALXP-TABLE         OCCURS 20.
               05  ALXP-EARN          PIC +9(7).99.
               05  FILLER             PIC X.

           03  ALXA-CODE              PIC X.
           03  FILLER                 PIC X.
           03  ALXA-SRCD              PIC XX.
           03  FILLER                 PIC X.
           03  ALXA-PMT-SCHED-CHNG OCCURS 9 TIMES.
               05  ALXA-START-PMT     PIC 99999.
               05  FILLER             PIC X.
               05  ALXA-END-PMT       PIC 99999.
               05  FILLER             PIC X.
               05  ALXA-NET-CHANGE    PIC +9(7).99.
               05  FILLER             PIC X.
           03  ALXA-LN-INSCOMP        PIC 99.
           03  FILLER                 PIC X.
           03  ALXA-LN-INSOURS        PIC X.
           03  FILLER                 PIC X.
           03  ALXA-LN-INSEFF-DATE    PIC 9(8).
           03  FILLER                 PIC X.
           03  ALXA-LN-INSEXP-DATE    PIC 9(8).
           03  FILLER                 PIC X.
           03  ALXA-LN-INSPREM        PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXA-LN-INSCOVR        PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXA-LN-INSCOMM        PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXA-LN-INS-REBATE     PIC X.
           03  FILLER                 PIC X.
           03  ALXA-LN-INSCODES       PIC X(3).
           03  FILLER                 PIC X.

           03  ALXZ-CODE              PIC X.
           03  FILLER                 PIC X.
           03  ALXZ-POST-REGPY        PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXZ-POST-EXTRA-PRIN   PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXZ-POST-OTH          PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXZ-POST-LCHG         PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXZ-PDUE-APINT        PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXZ-PDUE-APPRIN       PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXZ-CUR-APINT         PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXZ-CUR-APPRIN        PIC +9(7).99.
           03  FILLER                 PIC X.

           03  ALXR-CODE              PIC X.
           03  FILLER                 PIC X.
           03  ALXR-REB-AMT           PIC +9(7).99.
           03  FILLER                 PIC X.

           03  ALXX-CODE              PIC X.
           03  FILLER                 PIC X.
           03  ALXX-INS-TAX-COUNTY-CD PIC X(3).
           03  FILLER                 PIC X.
           03  ALXX-INS-TAX-CITY-CD   PIC X(3).
           03  FILLER                 PIC X.

           03  ALXM-CODE              PIC X.
           03  FILLER                 PIC X.
           03  ALXM-MIP-CLPAID        PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXM-MIP-CLPDTH-DATE   PIC 9(8).
           03  FILLER                 PIC X.
           03  ALXM-MIP-AHPAID        PIC +9(7).99.
           03  FILLER                 PIC X.
           03  ALXM-MIP-AHPDTH-DATE   PIC 9(8).
           03  FILLER                 PIC X.
           03  ALX-CR                 PIC X.
           03  ALX-LF                 PIC X.

