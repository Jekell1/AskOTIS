      * BAH 20230614 ADDED FILLER
      * 948 FIELDS               SIZE = 2724

      * REDUCED INSVERTBL TO OCCURS 5 FROM 14    IVER OCCURS 5 NOW

      * REDUCED INSTBL TO OCCURS 12 FROM 14      ILEV OCCURS 12 NOW

           03  SPB-AREA-B.

      *--------------------------------------------------------
      *    INSURANCE CALCULATION / VALIDATION TABLES
      *    LEVEL (1) - CL DEC             (8) - O1
      *          (2) - CL DEC JOINT       (9) - O2
      *          (3) - CL LEV            (10) - O3
      *          (4) - CL LEV JOINT      (11) - O4 NOT USED
      *          (5) - AH                (12) - O5 NOT USED
      *          (6) - PP                
      *          (7) - PP 2           
      *--------------------------------------------------------
      *ADDED 2 NEW INSURANCE LEVELS, O2/O3 ARE NOT CALCULATED BUT NEED
      *TO KEEP IN SYNC BECAUSE A WORKING STORAGE TABLE IN CALCZL USES
      *OCCURS 9&10 FOR INSURANCE TAX, 04/05 PRESENTLY NOT USED
               05 SPB-INSTBL                           OCCURS 12
                                                   INDEXED BY BILEV.
                   07 SPB-INSCOM                   PIC 99  COMP-3.
                       88 SPB-NO-INS-CO         VALUE 0.
                   07 SPB-INSRATECD                PIC 999 COMP-3.
                       88 SPB-INSRATECD-VALID   VALUE  1 THRU 25
                                                         71 THRU 99.
      * MAKE SURE TO UPDATE THIS IF THE RATECD IS "NET PAYOFF"
      * FOR INSURANCE TAPE CREATED BY SP/AMPLE.C
                       88 SPB-INSRATECD-NET-PAYOFF  VALUE
                                      08 10 12 13 14 15 16 17 18 19.

                       88 SPB-PER-100-PER-ANNUM     VALUE 1.
                       88 SPB-PER-1000-PER-MONTH    VALUE 2.
                       88 SPB-TABLE-INTERPOLATE     VALUE 3.
                       88 SPB-TABLE-ROUND-UP        VALUE 4.
                       88 SPB-VA-PC-ACTUARIAL78     VALUE 5.
                       88 SPB-VA-IB-ACTUARIAL       VALUE 6.
                       88 SPB-TX-PP-FIRE            VALUE 7.
                       88 SPB-CA-NETPAYOFF-5A       VALUE 8.
                       88 SPB-CA-NET-POFF-MULL      VALUE 9.
                       88 SPB-AL-NETPAYOFF          VALUE 10.
                       88 SPB-ADVANCE-MTG-FIRE      VALUE 11.
      * N O T E:SPB-INRATECD 10, OVERLAPS WITH RELEASE A48,
      *          IF AN UPGRADE EVER OCCURS, IT'S CODE MUST BE
      *          CHANGED IN THEIR SPR'S
      * IN A48, FAMILY USED 10, NOW MUST BE 12
                       88 SPB-VA-NET-POFF-FAMILY    VALUE 12.
                       88 SPB-RG-PA-NET             VALUE 13.
                       88 SPB-RG-OH-NET             VALUE 14.
                       88 SPB-RG-NY-NET             VALUE 15.
                       88 SPB-MAINE-NET             VALUE 16.
                       88 SPB-FREEDOM-NET-BALLOON   VALUE 17.
                       88 SPB-RG-TN-NET             VALUE 18.
                       88 SPB-RG-KY-NET             VALUE 19.
                       88 SPB-CA-TRUNC-NET-MULL     VALUE 20.
                       88 SPB-AL-NETPAY-PLUS1       VALUE 21.
                       88 SPB-IN-NET-PACE           VALUE 22.
                       88 SPB-100-ANNUM-EXTRA-30    VALUE 23.
                       88 SPB-AL-1000-PER-MONTH     VALUE 24.
                       88 SPB-INSRATECD-25          VALUE 25.
                       88 SPB-INS-RATE-FILE         VALUE 71 THRU 99.

                   07 SPB-INSJFAC                  PIC 9(4)V99  COMP-3.
                   07 SPB-INSDISCNT                PIC 99V99    COMP-3.
      *CHANGED FROM PIC X TO PIC X(2)
                   07 SPB-INSCHGABL                PIC X(2).
                       88 SPB-INSCHGABL-VALID   VALUE "3" "A" "B"
                                                         "C" "F" "H"
                                                         "I" "J" "K"
                                                         "L" "M" "N"
                                                         "O" "P" "R"
                                                         "S" "T" "V"
                                                         "X" "Y" "1"
                                                         "2" "D" "E".
                       88 SPB-INS-ADJ-PRIN-3PMTS    VALUE "3".
                       88 SPB-INS-AMTFIN-2PMTS      VALUE "2".
                       88 SPB-INS-AMTFIN-1PMTS      VALUE "1".
                       88 SPB-INS-ADJ-PRINCIPAL     VALUE "A".
                       88 SPB-INS-BENEFIT-TERM      VALUE "B".
                       88 SPB-INS-CASH-ADVANCE      VALUE "C".
                       88 SPB-INS-CHGABL-MAX-TERM   VALUE "D".
                       88 SPB-INS-MAX-TERM-BENEFIT  VALUE "E".
                       88 SPB-INS-CASH-ADV-FEES     VALUE "F".
                       88 SPB-INS-ADJ-PRIN-HALFINT  VALUE "H".
                       88 SPB-INS-ADJ-PRIN1INT      VALUE "I".
                       88 SPB-INS-ADJ-PRIN-HALFINT-TFC VALUE "J".
                       88 SPB-INS-TN-INT-SC-CL      VALUE "K".
                       88 SPB-INS-TN-INT-CL         VALUE "L".
                       88 SPB-INS-TOTAL-NOTE-EXT    VALUE "M".
                       88 SPB-INS-TOTAL-NOTE        VALUE "N".
                       88 SPB-INS-PRIN-CALC-MOB     VALUE "O".
                       88 SPB-INS-PRINCIPAL         VALUE "P" "O".
                       88 SPB-INS-ALL               VALUE "R".
                       88 SPB-INS-ADJ-CASH-ADVANCE  VALUE "S".
                       88 SPB-INS-NET-PAYOFF        VALUE "T".
                       88 SPB-INS-ADJ-PRIN-NO1INT   VALUE "V".
                       88 SPB-INS-SP-CASH-ADVANCE   VALUE "X".
                       88 SPB-INS-MIP               VALUE "Y".
                   07 SPB-INSRATETBL                   OCCURS 20
                                                   INDEXED BY BISTP.
                       09 SPB-INSTERM               PIC 9(3)     COMP-3.
                       09 SPB-INSRATE               PIC 99V9999  COMP-3.
      *            ----------------------------------------
                   07 SPB-INSMINPREM                PIC 9(4)V99  COMP-3.
                   07 SPB-INSMINCHGABL              PIC 9(6)     COMP-3.
                   07 SPB-INSMAXCHGABL              PIC 9(7)     COMP-3.
                       88 SPB-INSMAXCHGABL-OVRIDE   VALUE  88 88 888.
                   07 SPB-INSMAXBENIF               PIC 9(7)     COMP-3.
                   07 SPB-INSMAXTERM                PIC 9(3)     COMP-3.
                   07 SPB-INSCHGBEN                 PIC X.
                       88 SPB-INSCHGBEN-VALID       VALUE "B" "C".
                   07 SPB-INSEXCEPT                 PIC 99       COMP-3.
                       88 SPB-INSEXCEPT-VALID       VALUE 0 THRU 15.
                       88 SPB-SECURITY-INS-CO       VALUE 1.
                       88 SPB-EXTEND-INSURANCE      VALUE 2 12 15.
                       88 SPB-ADJ-INSCOV-BY-O2      VALUE 3 5.
                       88 SPB-ADJ-INSCOV-BY-O3      VALUE 4 5.
                       88 SPB-INSURE-EXT-PERIOD     VALUE 6.
                       88 SPB-MAX-OVERRIDE          VALUE 7.
                       88 SPB-EXTEND-EXPIRE         VALUE 7 8 13.
                       88 SPB-GERMANTOWN-CO         VALUE 9.
                       88 SPB-WDT-DUAL-EDIT         VALUE 10.
                       88 SPB-INS-UNITPER-MON       VALUE 11.
                       88 SPB-CALC-PREM-LESS-EXTCHG VALUE 12.
                       88 SPB-CL-AH-MAXIMUMS        VALUE 13 15.
                       88 SPB-ORGTERM-MAXIMUM       VALUE 14.
                   07 SPB-INSNOVER                  PIC 99       COMP-3.
                   07 SPB-INSVERTBL                    OCCURS 5
                                                   INDEXED BY BIVER.
                       09 SPB-INSMINAGE             PIC 99       COMP-3.
                       09 SPB-INSMAXAGE             PIC 99       COMP-3.
                       09 SPB-INSMAXCOV             PIC 9(7)     COMP-3.
                       09 SPB-INSMAXTRM             PIC 9(3)     COMP-3.
                   07 SPB-INSFEECD                  PIC XX.
                   07 SPB-INSFEE-FRMLA              PIC X.
                   07 SPB-INSCHGABL-FG              PIC X.
                       88 SPB-INSCHGABL-FG-VALID    VALUE " " "D".
                       88 SPB-INSCHGABL-DECLBAL     VALUE "D".
                   07 SPB-INSREJECT-FG              PIC X.
                       88 SPB-INSREJECT-FG-VALID    VALUE " " "A" "B"
                                                          "C" "D" "E"
                                                          "F".
                   07 SPB-INSFINCHG                 PIC X.
                       88 SPB-INSFINCHG-VALID       VALUE " " "Y".
                       88 SPB-INS-IS-FINCHG         VALUE "Y".
                   07  FILLER                       PIC X(15).
