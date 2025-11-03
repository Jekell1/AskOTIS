      * COPYMEMBER: LIBLP/LPPOFFW
      *-------------------------------------------------------------------------
      *             (LPPOFFW)
      *       PAYOFF ROUTINE WORKERS
      *
      * NOTE:
      *       THIS COPY MEMBER IS USED AS AN ARGUMENT IN A NUMBER OF CALLS.
      *       SO IF NEW FIELDS ARE ADDED, PUT THEM IN THE FILLER AREAS.
      *
      * REV:
      *  JTG 051089 CHANGED WORKERS TO COMP.
      *  BAH 090192 ADDED POFF-DEF-STOP-MESS FOR
      *             COLONIAL DEFERMENTS MESSAGE
      *  BAH 120392 ADDED POFF-RECAST-MESS FOR COSMO
      *             RECASTING TO ONLY DISPLAY MESSAGES
      *             WHEN TRULY RECASTING, NOT IN REPORTS
      *  JTG 022593 ADDED POFF-LPTRCD TO REFLECT PAYOFF TYPE
      *  MJD 051597 ADDED POFF-INT-REB-TBL. REFLECTS INTEREST REBATED FOR
      *             EACH INSURANCE TYPE.
      *  MJD 052097 ADDED A LEVEL TO POFF-INT-REB-TBL FOR FEES.
      *  JTG 060397 ADDED A LEVEL TO POFF-REBATE FOR 'O3' REBATE + 'O4'&'O5'
      *  JTG 061897 ADDED POFF-INSEFF-TEST & POFF-INSEXP-TEST TO BE USED
      *             BY LONPG7.C TO ISOLATE ADDON RECORDS PART OF CANCEL
      *             ADDED POFF-RTNCD WHICH IS TESTED IN LIBLP/LPPOF2 RE:LONPG7.C
      *  JTG 010598 ADDED POFF-RECAST-FG RE: IDEAL A48 TO A60
      *  JTG 072898 ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *  MJD 000119 ADDED POFF-NOLCHG (WORLD PR#218)
      *             NOT YET USED. MUST BE ENABLED BEFORE WORLD GOES TO A15
      *  JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *             ADDED LOGIC FOR INS-TAX                       WORLD #288
      *  JTG 020328 ADDED FILLER AREAS FOR FUTURE ADDITIONS TO THIS COPY
      *             MEMBER. THIS COPY MEMBER IS USED IN A NUMBER OF CALLS
      *             AND IS PASSED.                                WORLD #288
      *  JTG 020402 CHANGED POFF-INS-TAX TO POFF-INS-TAX-RB       WORLD #288
      *  JTG 020801 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  JTG 021220 ADDED POFF-LCAP-BATCH-REFCD POFF-LCAP-BATCH-REFCD-LC-FG
      *             ADDED NEW CODE FILE TYPE 'BR'-BATCH PAYMENT REFERENCE CODES
      *             BATCH PAYMENT REVISIONS                        REGACC #0006
      * MJD 160223 ADDED POFF-LXR-REB-AMT FOR GA EARLY PROVISION  REGMGT PR#4915
      *             THIS IS THE AMOUNT OF THE SERVICE CHARGE THAT
      *             IS THE GEORGIA 4% CLOSING FEE EARLY PROVISION CALCULATED 
      *             IN REBATE-EP54 (LIBGB/REBATE).
      *             THIS IS INCLUDED IN THE SERVICE CHARGE 
      *             REBATE BUT NEEDS TO BE BROKEN OUT ON CERTAIN CONTRACTS. 
      *             IT WILL BE STORED HERE IF NOT = 0
      *-------------------------------------------------------------------------
       01  POFF-WORKERS.

      * INPUT FIELDS:
           03  POFF-PAYDATE                PIC 9(8) COMP-3.
           03  POFF-MAKERCD                PIC X.
           03  POFF-LONPFA-UPD-FG          PIC X.
           03  POFF-DEF-STOP               PIC X.
           03  POFF-LPTRCD                 PIC XX.

           03  POFF-INSEFF-TEST            PIC 9(8) COMP-3 VALUE 0.
           03  POFF-INSEXP-TEST            PIC 9(8) COMP-3 VALUE 0.
           03  POFF-RECAST-FG              PIC X           VALUE " ".
           03  POFF-LCAP-BATCH-REFCD       PIC X(5)        VALUE " ".
           03  POFF-LCAP-BATCH-REFCD-LC-FG PIC X           VALUE " ".
           03  FILLER                      PIC X(25).

      * OUTPUT FIELDS:
           03  POFF-REBATE          OCCURS 13
                                     PIC S9(7)V99   COMP-3.
           03  POFF-INS-TAX-RB      OCCURS 8
                                     PIC S9(7)V99   COMP-3.
           03  POFF-OTDUE            PIC S9(7)V99   COMP-3.
           03  POFF-O2DUE            PIC S9(7)V99   COMP-3.
           03  POFF-LCDUE            PIC S9(7)V99   COMP-3.
           03  POFF-INTDUE           PIC S9(7)V99   COMP-3.
           03  POFF-NETDUE           PIC S9(7)V99   COMP-3 VALUE 0.
           03  POFF-PARTIALS         PIC S9(7)V99   COMP-3.
           03  POFF-LCPAID           PIC S9(7)V99   COMP-3.
           03  POFF-ADDON-INT-EARNED PIC S9(7)V99   COMP-3.
           03  POFF-ADDON-INT-REBATE PIC S9(7)V99   COMP-3.
           03  POFF-INT-REB-TABLE.
               05  POFF-INT-REB-TBL OCCURS 9
                                     PIC S9(7)V99   COMP-3.
           03  POFF-INT-REB-TBLX    REDEFINES POFF-INT-REB-TABLE.
               05  POFF-CL-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-AH-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-PP-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O1-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O2-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O3-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O4-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O5-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-FEES-INT-REB PIC S9(7)V99   COMP-3.
           03  POFF-NOLCHG                PIC S9(3)      COMP-3.
           03  POFF-LXM-MIP-CLPAID        PIC S9(7)V99   COMP-3.
           03  POFF-LXM-MIP-CLPDTH-DATE   PIC 9(8)       COMP-3.
           03  POFF-LXM-MIP-AHPAID        PIC S9(7)V99   COMP-3.
           03  POFF-LXM-MIP-AHPDTH-DATE   PIC 9(8)       COMP-3.
           03  POFF-LXR-REB-AMT           PIC S9(7)V99   COMP-3.
           03  FILLER                     PIC X(25).

      * WORKERS:
           03  POFF-SUB              PIC S99.
           03  POFF-ONE              PIC S9.
           03  POFF-SP-DISFRMLA      PIC X.
           03  POFF-RTNCD            PIC X.
           03  POFF-CLPREM-CHG-FG    PIC X.
           03  POFF-AHPREM-CHG-FG    PIC X.

      * FIELDS USED FOR CALLING SUBPROGRAMS:
           03  POFF-FORM-PATH.
               05  POFF-FORM-PROG          PIC X(8).
               05  POFF-FORM-PROG-NO       PIC X.
           03  POFF-COMMON.
               05  POFF-COMMON-RTNCD       PIC X.
               05  POFF-COMMON-INTADJ      PIC S9(7)V99 COMP.
               05  POFF-COMMON-AREA.
                   07  POFF-COMMON-PAYDATE PIC 9(8)     COMP.
                   07  POFF-ORIG-INTDUE    PIC S9(7)V99 COMP.
                   07  POFF-RECAST-MESS    PIC X.
                   07  FILLER              PIC X(6).
           03  FILLER                      PIC X(28).
