       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CALCZL.
       DATE-WRITTEN.    01/29/87.
       DATE-COMPILED. 25-FEB-88 16:17.
      ************************************************************************
      *                 (LP)
      *  DESCRIPTION:   DIRECT LOAN CALCULATION ROUTINES
      *
      *  ---------------------------------------------------------
      *  |                    N  O  T  E:                        |
      *  |                                                       |
      *  |       THIS PROGRAM USES A48 PAYMENT SCHEDULE LOGIC    |
      *  ---------------------------------------------------------
      *
      * REV:
      *  JTG 061195 ADDED SP-SCFRMLA = 20, MERCURY (MO)
      *  JTG 062195 EXPANDED FOR 10 INSURANCES
      *  JTG 062395 ADDED CALCULATION OF O1 INSURANCE
      *  JTG 070595 ADDED SP-SCFRMLA = 21, MERCURY, (MI)
      *  JTG 083195 ADDED SP-INSRATECD = 5, MERCURY ACTUARIAL 78THS (PC) (VA)
      *                   SP-INSRATECD = 6, MERCURY ACTUARIAL (IB) (VA)
      *  JTG 091995 ADD SP-INSEXCEPT = 12, MERCURY 'LA' SP-CALC-PREM-LESS-EXTCHG
      *  JTG 110395 CORRECTED SP-CALC-PREM-LESS-EXTCHG() PROBLEM WITH
      *             COVERAGE OVERRIDE
      *  JTG 110995 ADDED SP-INSCHGABLE = 'O', NC NET PAYOFF WITH CALCULATED MOB
      *  JTG 111595 ADDED RATECD SP-CA-NETPAYOFF-5A VALUE 8, ACTION CHARGABLE 5A
      *  BAH 011096 SUBTRACT COP-SERVICE-CHG FROM TOTAL NOTE WHEN
      *             CALCULATING "GA" SCHG ON LOANS > 18 MONTHS
      *  JTG 012696 ADDED RATECD SP-AL-NETPAYOFF VALUE 10, MERCURY ITT LYNDON
      *  BAH 042996 ADDED SCFRMLA = 22 FOR PACESETTER (MI)
      *  BAH 043096 ADDED SCFRMLA = 23, NORTH CAROLINA
      *             FLAT AMT, ONLY 2 ALLOWED WITHIN 12 MONTH PERIOD
      *  BAH 061196 CHANGED TO STATE OF "TN" MAINT FEE FRMLA 01., #E 124
      *  BAH 070896 ADDED SCFRMLA = 25 FOR MERCURY 'KS'
      *             IF REFINANCED WITHIN 6 MONTHS AND ORG STATE AND CLASS
      *             = PRLN, THE SCHG CAN ONLY BE ON NEW MONIES. MERCURY, 'KS'
      *  JFF 100996 ADDED SCFRMLA = 26 FOR MERCURY 'WA'
      *             IF REFINANCED WITHIN 4 MONTHS AND ORG STATE AND CLASS
      *             = PRLN, THE SCHG CAN ONLY BE ON NEW MONIES. MERCURY, 'WA'
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 111596 ADDED SP-INSRATECD = 11, (88) SP-ADVANCE-MTG-FIRE
      *  JTG 112196 CHANGED RATECD SP-AL-NETPAYOFF VALUE 10, MERCURY ITT LYNDON
      *             RE: SIZE ERROR ON 0 APR
      *  BAH 121396 ADDED SCFRMLA 27, MERCURY 'CA' #L 042
      *  BAH 121696 ADDED SCFRMLA 28, ADVANCE  # 405
      *  JTG 021297 ADDED THE FOLLOWING FOR MERCURY, OH 30 1 1
      *               88  SP-PYEXSPCODE-REGZ      VALUE 0.
      *               88  SP-PYEXSPCODE-REMAIN    VALUE 1.
      *             CORRECTED CEE-PYEXFRMLA-2OR7  TO USE SP-INTMETHOD
      *  JTG 021997 CORRECT BUG IN SCFRMLA = 21, WAS CALCULATING ON SCHG
      *  JTG 050697 ADDED USE OF SETCTBL COPY MEMBER
      *  JTG 072397 ADDED INSFINCHG & GL-TOTAL-FINCHG
      *  JTG 082697 ADDED GP-I-AM-WORLD FLAG, RE:  GPENV
      *  MJD 082997 MADE GP-I-AM-WORLD AN 88 LEVEL UNDER GP-COMPANY-FLAG
      *  JTG 103197 IF GP-I-AM-WORLD ADDED LOGIC TO FORCE ONE CALENDAR
      *             MONTH TO FIRST PAYMENT WHILE IN THIS CALCULATOR,
      *             LONPC2.C WILL RECOMPUTE THE APR AND EFFECTIVE RATE
      *             USING TRUE FIRST PAYMENT DATE
      *  JTG 111097 CHANGED ALL RATE COMPUTES TO 3 DECIMAL ACCURACY
      *             SINCE EXTERNAL CALCULATIONS OF RATES EX. LPAPRS
      *             ARE DONE TO 3 DECIMALS ACCURACY
      *  BAH 123097 ADDED SCFRMLA 29, SAME AS FRMLA 5 BUT WITH 4'S RESTRICTIONS
      *             MULLEN, PR# 589
      *  JTG 020498 REMOVED I-AM-WORLD RESTRICTION AND RESTRICTED THIS TO ONLY
      *             CAN LOANS, RE: LOGIC TO FORCE ONE CALENDAR MONTH TO FIRST
      *             PAYMENT, SEE ABOVE JTG 103197
      *  JTG 041798 ADDED LOGIC FROM A90, NEW TEST TO HOPEFULLY CATCH SC
      *             CONVERGE PROBLEM' 02/13/1995 JTG
      *   CS 042898 ADDED "RB" AND "RO" TO SERVICE CHARGE TEST FOR RENEWALS
      *  JTG 070998 ADDED LOGIC TO ALLOW 0 AND/OR < 0.011 RATE, SEE.. <LOW RATE>
      *  BAH 071498 SP-INSURE-EXT-PERIOD (SP-INSEXCEPT 6) WAS SUBTRACTING
      *             1 FROM THE TERM IF 1STPYDATE SAME MONTH AS INTDATE
      *  JTG 072198 ADDED SP-INSRATECD = 13, SP-LYNDON-NET-PAYOFF MERCURY, AL
      *  JTG 072198 REMOVED SP-INSRATECD = 13, SP-LYNDON-NET-PAYOFF MERCURY, AL
      *  JTG 100598 ADDED SP-CAL-BREAK-ON-FLATAMT    VALUE "F", WORLD 'MO'
      *  BAH 110398 ADDED SP-SCFRMLA 30, CITIZENS TN PR# 779
      *  JTG 121098 ADDED LOGIC TO TEST FOR BLENDED ADDON/DISCOUNT RATE
      *             AND TO NOT ROUND ON RECALCULATION OF PAYMENT
      *  JTG 122198 ADDED SP-SCFRMLA = 31, OHIO FLAT DOLLAR   , REGENCY #003
      *  JTG 122198 ADDED SP-SCFRMLA = 32, OHIO % PRINCIPAL   , REGENCY #004
      *  JTG 122198 ADDED SP-SCFRMLA = 33, OHIO %PRIN /FEE    , REGENCY #005
      *  JTG 122198 ADDED SP-SCFRMLA = 34, OHIO %CASH ADV.    , REGENCY #013
      *  BAH 021699 CHANGED SCFRMLA 33, RENEWED BETWEEN 91 AND 365 NO POINTS
      *             CAN BE TAKEN, BUT STILL TAKE ORIGINATION FEE, #018
      *  BAH 021899 ADDED SP-SCFRMLA = 35, PA RATE/$50. , REGENCY #016
      *  JTG 990218 ADDED SP-INSCHGABLE = 'F', CASH ADV LESS FEES, REGENCY #020
      *  JTG 990218 ADDED SP-INSRATECD = 13,14,15, REGENCY CL NET PAYOFF FRMLA'S
      *            88  SP-RG-PA-NET                VALUE 13.
      *            88  SP-RG-OH-NET                VALUE 14.
      *            88  SP-RG-NY-NET                VALUE 15.
      *  BAH 990222 ADDED SP-INSEXCEPT 13, REGENCY  (SP-CL-AH-MAXIMUMS) #023
      *  JTG 990224 CHANGED SP-SCFRMLA = 33, OHIO %PRIN /FEE, TO NOT COMPUTE
      *             STATE-WORKER (ORIG.FEE) WHEN SERVICE CHARGE AMOUNT IS
      *             INPUTED, REGENCY #005
      *  JGG 990225 CHANGED SP-INS-TN-INT-CL AND SP-INS-TN-INT-SC-CL
      *             CHARGEABLE CODES TO EXCLUDE CL PREMIUM IN COMPUTATION
      *             OF CHARGEABLE SINCE IT WAS CAUSING CALCUATOR TO
      *             NOT COVERAGE
      *  JTG 990317 CHANGED SP-INS-TN-INT-CL AND SP-INS-TN-INT-SC-CL
      *             TO INCLUDE WK-PRIOR-LOAN-BALANCE'S
      *  JTG 990409 FIX SCFRMLA 35 TO USE NEW MONIES ON PB IN 120   REGENCY #016
      *  JTG 990520 ADDED SP-MAINE-NET            VALUE 16          MAINE   #999
      *                   SP-FREEDOM-NET-BALLOON  VALUE 17        FREEDOM   #862
      *  BAH 990528 ALLOWED INSMAXBENIF TO WORK FOR O1 INSURANCE (8) REG #36
      *  BAH 990625 ADDED SCFRMLA 36, WESTERN PR#879
      *  JTG 990712 CORRECTED NEVER ENDING LOOP TO ZAA-CTT-31-32-33-SETUP
      *  BAH 990721 ADDED SCFRMLA 37, EXCEL PR# 894
      *  BLV 990914 EXCEL PR#951, SCFRMLA 37 APPLIES FOR SALES CONVERSIONS
      *  JTG 990915 CHANGED COPY LPAPRZWL TO LPAPRZLW
      *  BLV 990916 EXCEL PR#951, SCFRMLA 37 NO 180 DAY RESTRICTION FOR SC'S
      *  BAH 991014 ADDED SP-MFFRMLA 3, MERCURY PR# 220 TN AUDITOR
      *             AMT TO TEST BREAK ON SHOULD BE TOTAL REPAY...
      *  JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *             NO OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      *  BAH 000225 ADDED SP-SCFRMLA 38, REGENCY PR# 1095 FOR KY
      *             JUST LIKE SCFRMLA 35 EXCEPT NO RENEWAL TEST OR ROUNDING
      *  MJD 000228 ADD SP-INSRATECD 9, SP-CA-NET-POFF-MULL
      *  JTG 000308 ADDED LOGIC FOR UNION SECURITY CREDIT LIFE NET PAYOFF,
      *                    SP-RG-TN-NET & SP-RG-KY-NET                   REGENCY
      *  JTG 000316 CHANGED CIP-UNKNOWN-ERROR VALUE " " TO VALUE " " "%"
      *             CALCULATIONS VIA: POINTS ENTERED CAUSE SERVICE CHARGE TO
      *             EXCEED SPR MAX DURING INTERMIM CALCULATION AND SET
      *             MAXIMUM SCHG INPUTED BEFORE INTERATIONS SET TRUE SCHG
      *             WESTERN CREDIT PL: #243
      *  JTG 000403 CHANGED LOGIC FOR UNION SECURITY CREDIT LIFE NET PAYOFF,
      *                    SP-RG-TN-NET & SP-RG-KY-NET    REGENCY RE: DON MONROE
      *  MJD 000517 MADE THE FOLLOWING CHANGES (SUPPORT PR#1181)
      *             CHANGED SCFRMLA 33
      *             FOR 33
      *               1) CHARGED ON DIRECT LOANS, FIRST MORTGAGES, AND SECOND
      *                  MORTGAGES.
      *               2) CHARGE IS BASED ON PRINCIPLE AMOUNT WHICH INCLUDES
      *                  POINTS (ON MORTGAGES), BUT NOT THE ORIGINATION
      *                  CHARGE ITSELF.
      *               3) NO ORIGINATION FEE IS PERMITTED IF RENEWED WITHIN
      *                  90 DAYS. IF REFINANCED BETWEEN 91 AND A YEAR,
      *                  NO POINTS CAN BE TAKEN, BUT THE ORIGINATION
      *                  FEE MAY BE CHARGED
      *               4) THE CHARGE:
      *
      *   SCFRMLA 33 - SECURED LOANS       |    SCFRMLA 39 - UNSECURED LOANS
      *   -------------------------------- |    --------------------------------
      *   PRINCIPLE AMOUNT      CHARGE     |    PRINCIPLE AMOUNT      CHARGE
      *   -------------------   ---------- |    -----------------  -----------
      *      0.00  -   499.99    $  15.00  |     0.00  -   499.99  $  15.00
      *    500.00  -   999.99    $  30.00  |   500.00  -   999.99  $  30.00
      *   1000.00  -  1999.99    $  65.00  |  1000.00  -  1999.99  $  65.00
      *   2000.00 & OVER         > OF 1%   |  2000.00  -  4999.99  $ 100.00
      *                          OR $150   |  5000.00 & OVER       > OF 1%
      *                                    |                         OR $150
      *  JTG 000602 CORRECTED LOGIC TO MATCH LIBLP/LPAPRS, WHEN CALCULATING
      *             EFFECTIVE RATES IN EAA-CALC-ADDON & EAA-CALC-DISCOUNT
      *             FL DEALERS, JUDY
      *  JTG 000619 ADDED LOGIC FOR TENNESSEE RATE TYPE 'M' FLAT MONTHLY CHARGE
      *             #1225
      *  JTG 000710 CORRECTED LOGIC WHEN CALCULATING EFFECTIVE RATES IN
      *             EAA-CALC-ADDON & EAA-CALC-DISCOUNT
      *  JTG 000713 CORRECTED LOGIC FOR TENNESSEE RATE TYPE 'M' FLAT MONTHLY
      *             CHARGE, SOMETIMES SCHG WAS INCLUDED IN INTCHG #1225
      *  BAH 000831 CHANGED SCFRMLA 33 AS PER REGENCY, ADDED 1 MORE RATE
      *             BREAK AND CHANGED 2 AMOUNTS, PR# 1300
      *  BAH 000920 ADDED SCFRMLA 40 FOR REGENCY, AND TOTALLY CHANGED SCFRMLA
      *             39, THEY NEVER USED THE ORIGINAL CODE, PR# 1314
      *  MJD 001030 ADDED SCFRMLA 41, WESTERN PR#1344
      *  BAH 001215 CHANGED SCFRMLA 13, FOR NEW SOUTH "GA", PR# 1376
      *  BAH 010205 FIXED BUG WITH SCFRMLA 13, FAX FROM KEVIN
      *  JTG 010426 CHANGED RE: GEORGIA GILA CHANGE EFF: 04/13/01 WORLD PR#263
      *  BAH 010518 ADDED SCFRMLA 42 (SUNBELT) LIKE 18 BUT 360 YEAR, PR# 1484
      *  BAH 010607 ADDED SP-SCFRMLA = 43 FOR FCF "NC", PR# 1506
      *  BAH 010607 ADDED SP-SCFRMLA = 44 FOR FCF "VA", PR# 1498
      *  BAH 010620 ADDED SP-PYEXFRMLA = 11 FOR REGWAN, PR# 1515
      *  BAH 010629 CHANGED SP-SCFRMLA 35 TO ROUND UP TO NEXT WHOLE NUMBER
      *             BEFORE MULTIPLYING BY THE # OF 50$ UNITS. REGWAN PR#1532
      *  JTG 011102 CORRECTED DOUBLE DIP OF 365 FACTOR IN
      *             BDF-MANY-SIMPLES-AMOUNT-BREAK ROUTINE     PIKCO
      *  JTG 011106 NEW SP-INSRATECD (20) 88 SP-CA-TRUNC-NET-MULL MULLEN PR#1641
      *  BAH 011228 ADDED SP-SCFRMLA 45, HCRED "NC"  PR#1695
      *  JTG 020110 CHANGED LOGIC TO CALCULATE SIMPLE INTEREST TO MATCH
      *             LIBLP/LPAPRS BY ROUNDING CALCULATION OF APR-REGPYAMT
      *                                                            WORLD PR#J024
      *  JTG 020114 ADDED LOGIC OF LPAPRS TO MATCH SIMPLE RATE CALCULATION
      *                                                            WORLD PR#J024
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK PR#J021
      * JTG 020124 CHANGED XBA-CALCULATE-SIMPLE-INTEREST TO
      *             PERFORM APRS-GET-SIMPLE-RATE                   WORLD PR#J024
      * JTG 020301 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      * JTG 020510 CORRECTED REPLACING STATEMENT
      *            WAS :   REPLACING == VALUE ERO== BY ====.
      *            WITH:   REPLACING == VALUE ZERO== BY ====.
      * JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS    REGENCY #1801
      * BAH 020625 CHANGED SCFRMLA 35,39 AND 40 TO TEST PRIOR CLASS AND NEW
      *            CLASS TO BE  SMALL LOAN "Y" FOR RESTRICTIONS TO APPLY
      *            REGWAN PR# 1831
      * BAH 020806 ADDED SP-MFFRMLA 04 FOR NEW TN MAINTFEE RESTRICTIONS, JUST
      *            LIKE FRMLA 01 BUT CANT CHARGE ONE IF ANY OPEN LOANS ALREADY
      *            HAVE ONE, REGWAN PR# 1867
      * JTG 030120 CHANGED XBA-CALCULATE-SIMPLE-INTEREST ROUTINE TO SET
      *            RATE TO .005 WHEN INTEREST CHARGE IS INITIALLY ZERO,
      *            SETTING RATE TO ZERO CAUSED PROBLEMS FOR SOME INSURANCE
      *            CALC'S ON THE INITIAL COMPUTE          WORLD ALABAMA CLASS 5
      * JTG 030127 CHANGED LOGIC TO GET SIMPLE INTEREST CALCULATION TO TEST
      *            IF SP-PYEXFRMLA-REG-INCR FOR TEST
      * JTG 030128 CREATED FORMULA 12 LIKE 2 BUT SPREADS EXTENSION CHARGE
      *            OVER PAYMENTS  88  SP-PYEXFRMLA-SAME2-SPREAD VALUE 12.
      *                                                          CITIZENS #2006
      * BAH 030130 ADDED SP-SCFRMLA 46, LENDMARK  PR#2004
      * JTG 030314 TESTED FOR BLANKS IN ZBA-SET-INS-SWS AND CORRECTED
      *            BOGUS SET OF CIP.*SW()'S                      WORLD    #319
      * JTG 030527 CORRECTED TO FORCE 0 ON NON RENEWALS SP-SCFRMLA 46,
      *                                                       LENDMARK  PR#2004
      * BAH 030619 SCFRMLA 46 NEEDS TO TEST PRIOR STATE,CLASS AND SUBCLASS TO
      *            ALL MATCH NEW LOAN OR NO CHARGE CAN BE ASSESSED, LENDMARK
      *             PR# 2101
      * BAH 030619 A&H COVERAGE WAS NOT BEING TESTED BY SP-CL-AH-MAXIMUM
      *            IT WAS ON PURPOSE, BUT NOW THEY WANT IT, REGWAN PL# 410
      * BAH 030624 SCFRMLA 46, SOURCE "SC" ONLY THE STATE HAS TO MATCH
      * BAH 030807 FIXED A BAD 'IF' STATEMENT WITH 'AND' AND 'OR' FROM #1801
      *            ILEV = 2 CAUSED ENTIRE 'IF' TO BE TRUE
      *             ( SP-INS-MIP(ILEV) AND ILEV = 1 OR 2 OR 3 OR 4 )
      * BAH 031001 ADDED SP-INSCHGABLE '2' & '1',SP-INS-AMTFIN-2PMTS &
      *            SP-INS-AMTFIN-1PMTS FOR SIGNAL (WISC) PR# 2157
      * JTG 031202 ADDED FOR SIMPLE RATES SP-CAL-BREAK-ON-FLATAMT "F" WORLD J038
      * BAH 040331 SCFRMLA 43 AND 45 WERE NOT MOVING 0 TO SERVICE CHARGE IF
      *            RESTRICTED...QUOTE TOOK SERVICE CHARGE CAUSE IT DOESNT
      *            KNOW THE SSNO YET, THEN ENTER SSNO CALLS NCSC23 FOR
      *            RESTRICTIONS, IF RESTRICTED, CALCZL NEEDS TO MOVE 0 TO THE
      *            SERVICE CHARGE...SMITHFIELD PL# 438
      * JTG 040830 BUG FOR RATE TYPE 'SIMPLE' AND BREAK TYPE 'FLAT AMOUNT BREAK'      *            WAS USING LOGIC FOR RATE TYPE 'DISCOUNT' WHICH WAS FINDING
      *            A RATE IN THE TABLE BASED ON (CHARGEABLE + INTEREST)
      *                                                              WORLD J038X
      * JTG 041026 ADDED RATECD SP-AL-NETPAY-PLUS1       VALUE 21  WORLD  #J050
      * JTG 050224 ALLOW MEMO 'O3' INSURANCE AND CALCULATE KENTUCKY SALES TAX
      *                                                          LENDMARK #2478
      * JTG 050519 ADDED NEW FORMULAS FOR MEXICO
      *            SP-MFFRMLA-05 SP-RBFRMLA2 (G) SP-ERNFRMLA (24)   WORLD #0408
      * JTG 050718 ADDED RATECD SP-IN-NET-PACE           VALUE 22  PACE   #2469
      * JTG 050728 REMOVED 'IVA' MAINT FEE FROM PAYMENT AMOUNTS BEFORE
      *            COMPUTING THE APR                                WORLD #0408
      * BAH 050831 ADDED SP-SCFRMLA 47,  BAY COUNTRY PR# 2582
      * JTG 050915 ADDED TEST WHEN SP-MFFRMLA-05 TO CONFIRM THE LOAN IS
      *            IN BALANCE VIA COP-TOT-REPYBLE =
      *                           COP-AMT-FINANCD
      *                         + COP-SERVICE
      *                         + COP-MAINT-FEES
      *                         + COP-1ST-PY-EXT
      *                         + COP-INT-CHARGE
      *            WHEN LOAN IS IN BALANCE RETURN VIA AAA-VALID-RETURN
      *                                                              WORLD #0408
      * MJD 060831 ADDED LOGIC FOR NEW INSURANCE CHARGEABLE CODE "D"
      *            (SP-INS-CHGABLE-MAX-TERM).  THIS OPTION WILL USE THE
      *            MAX TERM FIELD TO CALCULATE COVERAGE WHEN THE LOAN TERM
      *            EXCEEDS IT.   PACESETTER #2771
      * MJD 060907 CORRECTED SCFRMLA 39 PROBLEM FOR REGENCY.
      *            THERE IS A CONVERGENCE ISSUE WHERE WHEN PAYMENTS ARE
      *            ROUNDED UP AND THE PROCEEDS ARE CLOSE TO THE BREAK
      *            POINT FOR THE SVC CHARGE, THE CALCULATOR SOMETIMES
      *            PRODUCES A RESULT THAT EXCEEDS THE MAXIMUM EFF-RATE
      *            FOR THE SPR.  ADDED TEST FOR SCFRMLA 39 IN THE FINAL
      *            CALCULATIONS.  IF THE RATE EXCEEDS THE MAX IN THE SPR
      *            THEN GO BACK THROUGH THE LOOP (WITHOUT RESETTING THE
      *            COUNTER) AND TRY TO COME UP WITH ANOTHER OUTCOME.
      *            ALONG WITH THE SAME REQUEST, THE RATE EXCEEDS MAXIMUM
      *            ERROR OCCURED WITH SCFRMLA 35.  THE CALCULATOR IS ROUNDING
      *            UP THE EFFRATE (ADDING .0009999) IN THE CALCULATION.
      *            THIS IS CAUSING RATES TO BE ROUNDED UP THAT PROBABLY
      *            SHOULD NOT.  (IE 9.5001000 WOULD BE ROUNDED TO 9.501
      *            INSTEAD OF 9.500). TALKED TO JOHN AND WE COULD NOT
      *            DETERMINE WHY THIS WAS IN THE LOGIC.  INSTEAD WE ADDED
      *            THE 'ROUNDED' FUNCTIONALITY (IE XXXX ROUNDED = YYY).
      *            ADDED RECALCULATION OF INT-CHARGEABLE AFTER CALCULATING
      *            A NEW SERVICE CHARGE.  THE INT-CHARGEABLE WAS NOT BEING
      *            RECALCUALTED EACH TIME THROUGH.  FOR SCFRMLA 39 THIS
      *            CAUSED THE EFF-RATE TO BE CALCULATED USING THE SCFRMLA
      *            FROM THE PREVIOUS LOOP THROUGH THE CALCULATOR. (I.E.
      *            EFF RATE WAS CALCULATED USING $100 SERVICE CHARGE WHEN
      *            THE FINAL LOOP CALCULATED A $200 SERICE CHARGE.)
      *                                                            REGENCY #0522
      * JTG 061205 ADDED SP-SCFRMLA = 48,(POINTS)FOR TN REAL ESTATE LOANS
      *                                                           REGENCY  #2880
      * JTG 061208 ALLOWED CHARGING OF INSURANCE SALES TAX FOR "FL" FLORIDA
      *                                                           LENDMARK #2875
      * MJD 070111 CHANGED WHERE SVCHG FRMLA 39 RECALCED INT CHARGEABLE (0522).
      *            ON RENEWALS IT WAS RECALCING BEFORE THE TEST FOR
      *            RENEWALS AND INCLUDING THE SVCCHG IN THE RECALC
      *            OF INT CHGABLE EVEN IF IT WAS RENEWED WITHING
      *            90 DAYS.  THIS WAS PREVENTING THE NUMBERS FROM
      *            CONVERTGING AND CAUSING AN ERROR TO BE GENERATED.
      *                                                           REGENCY PR#570
      * BAH 070730 ADDED SCFRMLA 49, JUST LIKE 29 EXCEPT CLASSES DONT HAVE TO
      *            MATCH FOR THE RESTRICTION, REGMGM PR# 3017
      * MJD 080519 CHANGED MFFRMLA-05 TO INCLUDE VALUE ADDED TAX (VAT)
      *            (I.E. MAINT FEES) IN THE CALCULATION
      *            OF THE APR.                                       WORLD #582
      * BAH 090821 ADDED SP-INSCHGABLE "E" FOR REGMGM PR# 3437
      * BAH 100301 ADDED SP-INSRATECD 23 FOR REGMGM PR# 3553
      * MJD 100311 ADDED SP-INSRATECD 24 FOR REGMGM PR# 3455
      * BAH 100420 CHANGED SP-INSRATECD 23 TO USE 365 TO DETERMINE
      *            ELAPSED DAYS TO 1STPYDATE, INSTEAD OF SPR YRTYPE
      * MJD 100712 CHANGED TEST FOR SP-MFMAX.  IF 9999.99 THEN TREAT AS
      *            NO LIMIT AND CONTINUE.  AS PER JR.  THIS IS AN ISSUE
      *            FOUND AS A RESULT OF MEXICO'SVAT TAX.
      * BAH 110106 ADDED SCFRMLA 50 FOR WORLD "IL", PR# 731
      * BAH 110119 REMOVE "FB" FROM SCFRMLA 47, BAY COUNTRY PR# 3714
      * BAH 110216 CHANGE SCFRMLA 50 FOR WORLD USING 3/21/11, PR# 731
      * BAH 110222 ADDED SCFRMLA 51 FOR WORLD "IL", PR# 746
      *  CS 110509 ADD TEST IF GP-I-AM-REGENCY & BR-STATE = 'KY' FORCE 'KY' TO
      *            CD-CY-STATE & CD-CT-STATE. WANTS ALL BORROWERS IN THE KY
      *            BRANCH TO BE CHARGED THE INSURANCE TAX REGARDLESS OF
      *            THE BORROWER STATE   REGENCY PR#3784
      * MJD 110517 ADDED LOGIC TO ROUND UP THE MAINT FEE TO BE EVENLY DIVISIBLE
      *            BY THE TERM IF MFFRMLA-05.  THIS IS TO PREVENT
      *            MX LOANS FROM GOING OUT OF BALANCE, ESP WHEN
      *            PROCEEDS ARE INPUT.   WORLD #692
      * MJD 110518 REMOVED ABOVE CHANGE  TO ROUND UP MFEE.  I WAS CAUSING
      *            SOME ISSUES WITH EXISTING CANNED LOANS SO JIM SAID TO
      *            REMOVE IT.   I LEFT THE CHANGES IN BUT COMMENTED THEM OUT
      *            SO THAT WE CAN REVISIT IT IN THE FUTURE.
      * BAH 110602 ADDED SP-SCFRMLA 52 AND 53 FOR MULLEN PR# 3799
      * MJD 110615 REINSTATED CHANGE TO ROUND UP MFEE FOR MFFRMLA-05, BUT
      *            TIME BASED IT ON SP-ROUND-MFEE.  THIS WAY EXISTING
      *            CAN LOANS CAN WORK AS THEY ALWAYS DID BUT NEW LOANS CAN
      *            USE THE ROUNDING FEATURE TO GET THEM IN BALANCE.    WORLD 692
      * BAH 110707 SCFRMLA 22 NEEDS TO ROUND DOWN, PACE PR# 3826
      * BAH 111109 SCFRMLA 47, CHANGE TO BE MORE LIKE 49, NEEDS ONLY 1 IN A
      *            12 MONTH TEST, BAY PR# 3898
      * MJD 120503 MODIFIED SP-INSRATECD = 24 TO EXCLUDE MAINT FEES
      *            FROM THE PREMIUM CALCULATION.                    REGMGT#4030
      * MJD 120713 CHANGED TO ONCE AGAIN EXCLUDE VAT FROM APR CAL IN MX
      *            PART OF CHANGES FOR                              WRLD PR#0811
      * MJD 120716 CHANGED APR CALC FOR MEXICO MFFRMLA-05. FOR FULL DESC
      *            SEE INLINE COMMENTS @ MX-ADJUSTED-APR SECTION.   WRLD PR#0811
      * BAH 130130 A30 CONVERSION
      * MJD 130409 REPLACED CALL TO MULSIML WITH VERYANTS COBOL VERSION OF
      *            PFS C ROUTINE.
      * MJD 130308 ADDED LOGIC FOR INSRATECD 25.                    REGMGT#4185
      * MJD 130530 ADDED LOGIC FOR SP-SCFRMLA = 54.  FUNCTIONS THE SAME AS 12
      *            BUT INCLUDES AN EXTRA 4% ADDED IN AT THE END BASED
      *            ON TOTAL NOTE                                     WORLD #0874
      * BAH 130814 ADDED SCFRMLA 55, CROSS BETWEEN 23 AND 32, WORLD "IN" PR# 918
      * BAH 130823 SCFRMLA 55, DONT TEST PRIOR CLASSES SAME, WORLD PR# 918A
      * BAH 131213 ADDED SCFRMLA 56, TX WORLD, PR# 953
      * BAH 150508 ADDED SCFRMLA 57, NSA PR# 4747
      *
      * KEC 2015.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         WITHIN LIBCL/EBCASC FOR USE WITH MICROFOCUS.
      * BAH 160427 ADDED SCFRMLA 58, WORLD PR#1154
      * BAH 190131 CHANGED LP01CD TO HARD CODED TO PASS TO INSTAX
      * BAH 20190529 ADDED SCFRMLA 59, WORLD "KY" PR#1374
      * BAH 20200702 CHANGED SCFRMLA 55, NEED A RATE OF $150 AND SP-SCRATE CAN
      *              ONLY HOLD 99.99, SO USE SP-SCMAX AS THE RATE, #1446
      * BAH 20210615 ADDED SCFRMLA 60 & 61, WORLD "TN" #1500
      * BAH 20220105 IF ERROR DUE TO APR BELOW MIN OR ABOVE MAX, DISPLAY
      *              ERROR MESSAGE "APR RATE EXCEEDS MAXIMUM IN SPR", OR
      *              MINIMUM, MOVE "C" TO OVR-RATE-SW, THEY DO NOT WANT TO FORCE
      *              THE OVR RATE WITH THE "A" APR. RETURN TO XONPC0 WITH "O"
      *              IN THE COP-RETURN-CODE SO ITS GIVES ERROR IN CALCULATIONS
      *              AND FORCES THEM TO FIX THE LOAN #1533
      * BAH 20230608 ADDED SCFRMLA 62, "TN" WITH CLOSING FEE #1608
      * JKC 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      * BAH 20241010 ADDED SCFRMLA 63, "WI" #1682 S35Q-141
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION  SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE  SECTION.
      ****************************************
       WORKING-STORAGE  SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/CALCZL S35 03/27/25-10:21:22 >".
      ****************************************

       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)LP/CALCZL A15 06/19/02-11:53:06 06/25/02-10:25:41 >".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
      ******************************************************************
      *    COPIED FROM LP01CD
      *
      *    CCY -  CITY INSURANCE TAX RATES FOR KENTUCKY
      *
      *    LEVEL: 1  CL DEC             8  O1
      *           2  CL DEC JOINT       9  O2
      *           3  CL LEV            10  O3
      *           4  CL LEV JOINT      11  O4
      *           5  AH                12  O5
      *           6  PP
      *           7  PP 2
      ******************************************************************
       01  CD-CCY-REC.
           03  CD-CCY-KEY.
               05  CD-CCY-TYPE                   PIC X(2).
               05  CD-CCY-STATE                  PIC X(2).
               05  CD-CCY-CITY                   PIC X(3).
               05  CD-CCY-FILLER                 PIC X(3).
      *-----------------------------------------------------------------
           03  CD-CCY-INSTBL                     OCCURS 12.
               05  CD-CCY-INSTAXFRMLA            PIC X.
               05  CD-CCY-INSTAXAMT              PIC 99V99 COMP-3.
           03  CD-CCY-BOTH-TAX-FG                PIC X.

       01  CD-CCT-REC.
           03  CD-CCT-KEY.
               05  CD-CCT-TYPE                   PIC X(2).
               05  CD-CCT-STATE                  PIC X(2).
               05  CD-CCT-COUNTY                 PIC X(3).
               05  CD-CCT-FILLER                 PIC X(3).
      *-----------------------------------------------------------------
           03  CD-CCT-INSTBL                     OCCURS 12.
               05  CD-CCT-INSTAXFRMLA            PIC X.
               05  CD-CCT-INSTAXAMT              PIC 99V99 COMP-3.
           03  CD-CCT-BOTH-TAX-FG                PIC X.

       01  ERRCD                PIC X        VALUE SPACES.
       01  BLENDED-ADDON-DISC-FG    PIC X.

       01  WS-RATE              PIC 9V99     VALUE 0.
       01  WS-WORKER            PIC S9(7)V99 COMP-3.
       01  SAVE-CIP-LOANDATE    PIC 9(8)     COMP.
       01  SAVE-CIP-INTDATE     PIC 9(8)     COMP.
       01  SAVE-CIP-1STPYDATE   PIC 9(8)     COMP.
       01  APRS-PYAMT-ADJ       PIC S9(7)V99 COMP.
       01  SAVE-POINTS          PIC 99V99    COMP.
       01  MAX-POINTS           PIC 99V99    COMP.
       01  MAX-SVCCHG           PIC 9(7)V99  COMP.
      ******************************************************************
      *                                                                *
      *  FOLLOWING IS THE PROGRAM INTERMEDIATE WORK AREA               *
      *                                                                *
      ******************************************************************
       01  WKS-WORKING-STORAGE-AREA.
           05  SUB                       PIC 9(5)          COMP.
           05  CVA-A--N                  PIC S9999V9(13)   COMP.
           05  CVA-SP                    PIC S999999V9(11) COMP.
           05  CVA-SP3                   PIC S999999V9(3)  COMP.
           05  CVA-I                     PIC S999999V9(11) COMP.
           05  CVA-WK                    PIC S999999V9(11) COMP.
           05  CVA-LP                    PIC S9(11)V9(6)   COMP.
           05  CVA-D                     PIC S9999V9(13)   COMP.
           05  CVA-M                     PIC 9(3)          COMP.
           05  CVA-N                     PIC 9(3)          COMP.
           05  CVA-V                     PIC S9999V9(13)   COMP.

           05  RG-K-DAYS                 PIC 9(3)          COMP.
           05  RG-E                      PIC 9(3)          COMP.
           05  RG-N                      PIC 9(3)          COMP.
           05  RG-M                      PIC 9(3)          COMP.
           05  RG-N-1                    PIC 9(3)          COMP.
           05  RG-A--N                   PIC S9999V9(13)   COMP.
           05  RG-A--M                   PIC S9999V9(13)   COMP.
           05  RG-A--M-N                 PIC S9999V9(13)   COMP.
           05  RG-A--N-1                 PIC S9999V9(13)   COMP.
           05  RG-SPR                    PIC S999999V9(11) COMP.
           05  RG-I                      PIC S999999V9(11) COMP.
           05  RG-WK                     PIC S999999V9(11) COMP.
           05  RG-FAC1                   PIC S999999V9(11) COMP.
           05  RG-FAC2                   PIC S999999V9(11) COMP.
           05  RG-NUM                    PIC S999999V9(11) COMP.
           05  RG-DEN                    PIC S999999V9(11) COMP.
           05  RG-DR                     PIC S999999V9(11) COMP.

           05  WKS-ALL-INS-PREMIUMS      PIC S9(7)V99    COMP.
           05  WKS-ALL-INS-TAX           PIC S9(7)V99    COMP.
           05  WKS-YRTYPE-ADJUST         PIC S9999V9(13) COMP.
           05  WKS-ANNUITY               PIC S9999V9(13) COMP.
           05  WKS-BAH-ANNUITY           PIC S9999V9(13) COMP.
           05  WKS-INS-ANNUITY           PIC S9999V9(13) COMP.
           05  WKS-INSRATE-1000          PIC S99V9(4)    COMP.
           05  WKS-94V913                PIC S9999V9(13) COMP.
           05  WKS-INS-FINCHG            PIC S9(7)V99    COMP.
           05  WKS-REGPYAMT              PIC S9(7)V99   COMP.
           05  WKS-PRINCIPAL             PIC S9(7)V99   COMP.
           05  WKS-REG-PAYMENT           PIC S9(7)V99   COMP
                                         VALUE 0.
           05  WKS-ODD-1ST-PMT           PIC S9(7)V99   COMP
                                         VALUE 0.
           05  WKS-MTHLY-MAINT-FEES      PIC S9(7)V99   COMP
                                         VALUE 0.
           05  WKS-PROCEEDS              PIC S9(7)V99   COMP.
           05  WKS-PRIOR-LOAN-BALANCE    PIC S9(7)V99   COMP.
           05  WKS-TOTPRIOR-LOAN-BALANCE PIC S9(7)V99   COMP.
           05  WKS-910V98                PIC S9(10)V9(8) COMP.
           05  WKS-98V99                 PIC S9(8)V9(9) COMP.
           05  WKS-98V93                 PIC S9(8)V9(3) COMP.
           05  WKS-98V92                 PIC S9(8)V9(2) COMP.
           05  WKS-GO-TO                 PIC 99.
           05  WKS-BAH-RATE              PIC S9999V9999 COMP.
           05  WKS-RATE                  PIC S9999V9999 COMP
                                         VALUE 0.
           05  WKS-RATE2                 PIC S9999V9999 COMP.
           05  WKS-APR-LOOP-COUNTE       PIC S9         COMP
                                         VALUE 0.
           05  WKS-RATE-CEF              PIC SV999      COMP.
           05  WKS-RATE-CEE              PIC S9999V9999 COMP.
           05  WKS-RATE-CEI              REDEFINES WKS-RATE-CEE
                                         PIC S9999V9999 COMP.
           05  WKS-HOLD-EFFRATE          PIC S9999V9999 COMP.
           05  WKS-CHARGE-PER-MONTH      PIC S9999V999  COMP.
           05  WKS-UNIT-CEH              PIC S9(5)V9999 COMP.
           05  WKS-DISFRMLA              PIC X.
           05  WKS-ROUNDV99              PIC S9999V99   COMP.
           05  WKS-ROUNDV999             PIC S9999V999  COMP.
           05  WKS-ALL-NINES             PIC S9(7)V99   COMP
                                         VALUE 9999999.99.
           05  WKS-INT-DAYS              PIC S99        COMP.
           05  WKS-TERM-1                PIC S9(4)      COMP.
           05  WKS-TERM-2                PIC S9(4)      COMP.
           05  WKS-TERM-3                PIC S9(4)      COMP.
           05  WKS-TERM-4                PIC S9(4)      COMP.
           05  WKS-TEST-AMOUNT           PIC S9(9)V99   COMP.
           05  WKS-AMOUNT                PIC S9(7)V99   COMP.
           05  WKS-AMOUNT-1              PIC S9(7)V99   COMP.
           05  WKS-AMOUNT-2              PIC S9(7)V99   COMP.
           05  WKS-AMOUNT-3              PIC S9(7)V99   COMP.
           05  WKS-AMOUNT-4              PIC S9(7)V99.
           05  WKS-AMOUNT-4X REDEFINES WKS-AMOUNT-4.
               10  WKS-AMOUNT-4-WHOLE    PIC S9(7).
               10  WKS-AMOUNT-4-DEC      PIC 99.
           05  WKS-WORKER                PIC S9(7)V99   COMP.
           05  WKS-WORKER2               PIC S9(7)V99   COMP.
           05  WKS-CY-TAX                PIC S9(7)V99   COMP.
           05  WKS-CT-TAX                PIC S9(7)V99   COMP.
           05  WKS-MTHS                  PIC S999       COMP.
           05  WKS-INTMETHOD-DAYS        PIC S999       COMP.
           05  WKS-CTD-NEW-MONEY-SW      PIC X VALUE "0".
               88  WKS-CTD-USE-NEW-MONEY       VALUE "1".
           05  WKS-CTG-MONEY             PIC S9(7)V99 COMP-3.
           05  WKS-CTG-USE-HALF-SW       PIC X VALUE "0".
               88  WKS-CTG-USE-HALF            VALUE "1".
           05  WKS-CTG-NEW-MONEY-SW      PIC X VALUE "0".
               88  WKS-CTG-USE-NEW-MONEY       VALUE "1".
           05  WKS-CTJ-SVCCHG-SW         PIC X VALUE "1".
               88  WKS-CTJ-SVCCHG-OK           VALUE "1".
           05  WKS-CTS-SVCCHG-SW         PIC X VALUE "1".
               88  WKS-CTS-SVCCHG-OK           VALUE "0".
           05  WKS-CTU-PRIOR-MONEY       PIC S9(7)V99 COMP-3.
           05  WKS-CTU-AMOUNT            PIC S9(7)V99 COMP-3.
           05  WKS-CTU-8-PERCENT         PIC S9(7)V99 COMP-3.
           05  WKS-CTU-4-PERCENT         PIC S9(7)V99 COMP-3.
           05  WKS-CTU-EXTRA-4-PERCENT   PIC S9(7)V99 COMP-3.
           05  WKS-CTU-NEW-MONEY-SW      PIC X VALUE "0".
               88  WKS-CTU-USE-NEW-MONEY       VALUE "1".
           05  WKS-CQA-MAINT-FEE-SW      PIC X VALUE "1".
               88  WKS-CQA-MAINT-FEE-OK        VALUE "1".
           05  WKS-SPAY-FG               PIC X VALUE "N".
           05  WKS-NPR-PPY               PIC 9(2).
           05  WKS-NPR-R                 PIC 9(3)V9(4).


       01  INSURANCE-WORKERS.
           05  CVM-A--P-J                PIC S9999V9(13)   COMP.
           05  CVVA-A--N-I              REDEFINES CVM-A--P-J
                                         PIC S9999V9(13)   COMP.
           05  CA-A--N-T-I              REDEFINES CVM-A--P-J
                                         PIC S9999V9(13)   COMP.
           05  AL-A--N-T-I              REDEFINES CVM-A--P-J
                                         PIC S9999V9(13)   COMP.

           05  CVM-A--P-I                PIC S9999V9(13)   COMP.
           05  CVVA-A--N-M-I            REDEFINES CVM-A--P-I
                                         PIC S9999V9(13)   COMP.
           05  CA-A--N-I                REDEFINES CVM-A--P-I
                                         PIC S9999V9(13)   COMP.
           05  AL-A--N-I                REDEFINES CVM-A--P-I
                                         PIC S9999V9(13)   COMP.
           05  IN-A--N-I                REDEFINES CVM-A--P-I
                                         PIC S9999V9(13)   COMP.

           05  CVM-I                     PIC S999999V9(11) COMP.
           05  CVVA-I                   REDEFINES CVM-I
                                         PIC S999999V9(11) COMP.
           05  CA-I                     REDEFINES CVM-I
                                         PIC S999999V9(11) COMP.
           05  AL-I                     REDEFINES CVM-I
                                         PIC S999999V9(11) COMP.
           05  IN-I                     REDEFINES CVM-I
                                         PIC S999999V9(11) COMP.

           05  CVM-J                     PIC S999999V9(11) COMP.
           05  CA-K                     REDEFINES CVM-J
                                         PIC S999999V9(11) COMP.

           05  CVM-P                     PIC 9(3)          COMP.
           05  CVVA-M                   REDEFINES CVM-P
                                         PIC 9(3)          COMP.
           05  CA-T                     REDEFINES CVM-P
                                         PIC 9(3)          COMP.
           05  AL-T                     REDEFINES CVM-P
                                         PIC 9(3)          COMP.
           05  IN-T                     REDEFINES CVM-P
                                         PIC 9(3)          COMP.

           05  CVM-N                     PIC 9(3)          COMP.
           05  CVVA-N                   REDEFINES CVM-N
                                         PIC 9(3)          COMP.
           05  CA-N                     REDEFINES CVM-N
                                         PIC 9(3)          COMP.
           05  AL-N                     REDEFINES CVM-N
                                         PIC 9(3)          COMP.
           05  IN-N                      PIC 9(3)          COMP.
           05  IN-OD                     PIC 9(3)          COMP.
           05  IN-SP                     PIC S999999V9(11) COMP.
           05  IN-SP-FOR-AH              PIC S999999V9(11) COMP.
           05  IN-SP1                    PIC S999999V9(11) COMP.
           05  IN-SP2                    PIC S999999V99    COMP.
           05  IN-OD30                    PIC S999999V9(11) COMP.

           05  CVM-WK                    PIC S999999V9(11) COMP.
           05  CVVA-WK                  REDEFINES CVM-WK
                                         PIC S999999V9(11) COMP.
           05  CA-E                     REDEFINES CVM-WK
                                         PIC S999999V9(11) COMP.
           05  IN-ODF                   REDEFINES CVM-WK
                                         PIC S999999V9(11) COMP.
           05  IN-RATE                   PIC S99V9(4)      COMP.

           05  IN-K                      PIC S999999V99 COMP.
           05  CA-W                      PIC S999999V9(11) COMP.
           05  CA-B                      PIC S999999V9(11) COMP.
           05  CA-G                      PIC S999999V9(11) COMP.
           05  CA-S                      PIC S999999V9(11) COMP.

       01  IWA-INSURANCE-WORK-AREA.
           05  IWA-INS-CHARGEABLE              PIC S9(7)V99    COMP-3.
           05  IWA-INS-BENEFIT                 PIC S9(7)V99    COMP-3.
           05  IWA-INS                         PIC S9(7)V99    COMP-3.
           05  IWA-EXT-DAYS                    PIC S9(5)       COMP-3.
           05  IWA-RATE                        PIC S9999V9999  COMP-3.
           05  IWA-TERM                        PIC S9(4)       COMP-3.
           05  IWA-LOAN-ANNUITY                PIC S9999V9(13) COMP-3.
           05  IWA-INS-ANNUITY                 PIC S9999V9(13) COMP-3.

       01  INT-INTEREST-WORK-AREA.
           05  INT-LO                          PIC S9999V99   COMP-3.
           05  INT-HI                          PIC S9999V99   COMP-3.
           05  INT-MID                         PIC S9999V99   COMP-3.
           05  INT-VALUE                       PIC S9(9)V9(9) COMP-3.
           05  INT-MTHS                        PIC S99        COMP-3.
           05  INT-MTHS-POS                    PIC S99        COMP-3.
           05  INT-DAYS                        PIC S99        COMP-3.

       01  HOLD-INS-CHARGEABLE                 PIC S9(7)V99   COMP-3.
       01  HOLD-IWA-INS                        PIC S9(7)V99   COMP-3.
       01  CTX-INSRATE                         PIC S9(3)V99   COMP-3.
       01  POINTS-TEST-MAX-FG                  PIC X.

      *-----------------------------------------------------------------
      *  WORKERS USED TO CORRECT A PROBLEM WITH MX VAT TAX.
      *  WHEN MAINTFEE IS CALCULATED AND IT IS NOT EVENLY DIVISIBLE BY
      *  THE TERM, WE MAY BE OPUT OF BALANCE.
      *  THIS IS ESP TRUE OF THE PROCEEDS ARE INPUT.
      *  THESE WORKERS ARE USED TO FIND OUT HOW MUCH IS NEEDED
      *  TO BE ADDED TO THE MAINT FEE IN ORDER TO MAKE IT EVENLY
      *  DIVISIBLE BY THE TERM.
      *-----------------------------------------------------------------
       01  MXMF-WORKER                         PIC 9(6)V99    COMP.
       01  MXMF-REMAINDER                      PIC 9(3)V99    COMP.
       01  MXMF-OFFSET                         PIC 9(6)V99    COMP.

      ******************************************************************

       COPY "LIBLP/MULSIML.CPY".
       COPY "LIBLP/LPWKCMPL.CPY".
       COPY "LIBLP/LPWKINDEX.CPY".

       01  CPP-CALCULATION-PRIOR-AREA.
           05  CPP-PROCEEDS                PIC 9(7)V99 COMP-3.
           05  CPP-REG-PAYMENT             PIC 9(7)V99 COMP-3.
      *-----------------------------------------------------------------

      ******************************************************************
      *                                                                *
      *  FOLLOWING ARE ALL OF THE MEMBERS THAT ARE COPIED FROM THE     *
      *  COMMON LIBRARIES.                                             *
      *                                                                *
      ******************************************************************
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LPAPRZLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBGB/GB01GP.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPWKCIPL.CPY".
       COPY "LIBLP/LPWKCEPL.CPY"
           REPLACING == VALUE ZERO== BY ====.
       COPY "LIBLP/LPWKCOPL.CPY".
      ****************************************************
      *    SP-REC - STATE PARAMETER FILE RECORD          *
      *    NOTE:                                         *
      *     IF THE PICTURE OF SP-YEARTYPE OR SP-REBTBL   *
      *            CHANGES, CHANGE LPEARN ALSO           *
      ****************************************************
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       01  BW-STATE                PIC X(2).
       01  BW-CITY-TAX-CD          PIC X(3).
       01  BW-COUNTY               PIC X(3).


       PROCEDURE DIVISION  USING
           FORM-PATHNAME
           CIP-CALCULATION-INPUT-AREA
           CEP-CALCULATION-EDIT-AREA
           COP-CALCULATION-OUTPUT-AREA
           SP-REC
           BR-REC BW-STATE BW-CITY-TAX-CD BW-COUNTY.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       AAA-MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".

           PERFORM MAA-INPUT-EDIT.
           PERFORM ZAA-INITIALIZE.
           IF COP-BAD-INPUT
              GO TO AAA-EXIT.

      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS FLAG IS SET TO 'N' TO STOP TESTING CALCULATED
      *    SERVICE CHARGE FROM INPUTED POINTS, UNTIL EAA-FINAL-CALCULATIONS
      *    DETERMINES THERE IS A VIOLATION FROM SPR'S AND SETS IT TO A 'Y'
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO POINTS-TEST-MAX-FG.
      *    - - - - - - - - - - - - - - - - - - - - - -

       AAA-RESTART-IF-BAD-RATE.
           MOVE 0 TO COP-LOOP-COUNT.

       AAA-LOOP.
           ADD 1 TO COP-LOOP-COUNT.
           PERFORM ABA-CALCULATION.
           IF NOT COP-UNKNOWN-ERROR
              GO TO AAA-EXIT.

           IF (COP-LOOP-COUNT = COP-LOOP-COUNT-MAX)
              GO TO AAA-FINAL-CALCULATIONS.

           IF (CMP-WORKING-AMOUNTS = COP-WORKING-AMOUNTS)
              IF (CMP-SCREEN-FIELDS = COP-SCREEN-FIELDS)
                 GO TO AAA-FINAL-CALCULATIONS.

           IF COP-LOOP-COUNT NOT > 10
              GO TO AAA-LOOP.

           COMPUTE WKS-TEST-AMOUNT = CMP-REG-PAYMENT + .01.
           IF COP-REG-PAYMENT-IS-CALCD
              IF COP-REG-PAYMENT = WKS-TEST-AMOUNT
                 IF COP-REG-PAYMENT = CPP-REG-PAYMENT
                    GO TO AAA-FINAL-CALCULATIONS.

           COMPUTE WKS-TEST-AMOUNT = CMP-PROCEEDS - COP-TERM * .01.
           IF COP-PROCEEDS-ARE-CALCD
              IF COP-PROCEEDS > WKS-TEST-AMOUNT
                 IF COP-PROCEEDS < CMP-PROCEEDS
                    IF COP-PROCEEDS = CPP-PROCEEDS
                       GO TO AAA-FINAL-CALCULATIONS.

      * NEW TEST TO WHEN SP-MFFRMLA-05 MEXICO
           IF COP-LOOP-COUNT > 50
              IF SP-MFFRMLA-05
                 COMPUTE WKS-WORKER =
                     COP-AMT-FINANCD + COP-SERVICE-CHG
                   + COP-MAINT-FEES  + COP-1ST-PY-EXT + COP-INT-CHARGE
                 IF COP-TOT-REPYBLE = WKS-WORKER
                    GO TO AAA-FINAL-CALCULATIONS
                 ELSE
                    GO TO AAA-LOOP.

      * NEW TEST TO HOPEFULLY CATCH SC CONVERGE PROBLEM' 02/13/1995 JTG
           IF COP-LOOP-COUNT > 50
              IF COP-PROCEEDS-ARE-CALCD
                 IF COP-PROCEEDS = CMP-PROCEEDS
                    GO TO AAA-FINAL-CALCULATIONS.

           GO TO AAA-LOOP.

       AAA-FINAL-CALCULATIONS.
           PERFORM EAA-FINAL-CALCULATIONS.
           IF NOT COP-UNKNOWN-ERROR
              GO TO AAA-EXIT.

           IF COP-RETURN-CODE = "%"
              MOVE " " TO COP-RETURN-CODE
              GO TO AAA-RESTART-IF-BAD-RATE.

           IF COP-OVR-RATE-IS-NULL
              PERFORM EDA-CHECK-MAX-RATES
      * COP-RETURN-CODE - "O"
              IF COP-APR-ERR
      * IF ERROR DUE TO APR BELOW MIN OR ABOVE MAX, MOVE "C" TO OVR-RATE-SW AND
      * LEAVE THE "O" TO RETURN TO XONPC0 WITH ERROR. #1533
                 IF COP-OVR-RATE-SW = "A" 
                    MOVE "C" TO COP-OVR-RATE-SW
                    GO TO AAA-ERROR-EXIT
                 ELSE
                    MOVE " " TO COP-RETURN-CODE
                    GO TO AAA-RESTART-IF-BAD-RATE.

      *  WE DO NOT WANT TO RESET THE LOOP COUNTER HERE.
      *  WE ONLY WANT IT TO LOOP THROUGH 1-3 MORE TIMES, RESETTING
      *  THE COUNTER WILL LOOP TO MUCH AND RETURN THE SAME RESULT
      *  CREATING AN INFINITE LOOP. /MJD 20060630 REGENCY PR#522
           IF SP-SCFRMLA = 39
              PERFORM EDA-CHECK-MAX-RATES
              IF COP-APR-ERR
                 MOVE " " TO COP-RETURN-CODE
                 GO TO AAA-LOOP.

           IF NOT COP-UNKNOWN-ERROR
              GO TO AAA-EXIT.

           IF (CMP-SCREEN-FIELDS = COP-SCREEN-FIELDS)
              IF (CMP-WORKING-AMOUNTS = COP-WORKING-AMOUNTS)
                 GO TO AAA-VALID-RETURN.

           IF COP-LOOP-COUNT NOT > 10
              GO TO AAA-ERROR-EXIT.

           COMPUTE WKS-TEST-AMOUNT = CMP-REG-PAYMENT + .01.
           IF COP-REG-PAYMENT-IS-CALCD
              IF COP-REG-PAYMENT = WKS-TEST-AMOUNT
                 IF COP-REG-PAYMENT = CPP-REG-PAYMENT
                    GO TO AAA-VALID-RETURN.

           COMPUTE WKS-TEST-AMOUNT = CMP-PROCEEDS - COP-TERM * .01.
           IF COP-PROCEEDS-ARE-CALCD
              IF COP-PROCEEDS > WKS-TEST-AMOUNT
                 IF COP-PROCEEDS < CMP-PROCEEDS
                    IF COP-PROCEEDS = CPP-PROCEEDS
                       GO TO AAA-VALID-RETURN.

      * NEW TEST TO HOPEFULLY CATCH SC CONVERGE PROBLEM' 02/13/1995 JTG
           IF COP-LOOP-COUNT > 50
              IF COP-PROCEEDS-ARE-CALCD
                 IF COP-PROCEEDS = CMP-PROCEEDS
                    GO TO AAA-VALID-RETURN.


       AAA-ERROR-EXIT.
           IF SP-MFFRMLA-05
              MOVE COP-LOAN-AMOUNT TO CMP-LOAN-AMOUNT
              MOVE COP-TOT-REPYBLE TO CMP-TOT-REPYBLE
              GO TO AAA-VALID-RETURN.

           MOVE "C" TO COP-RETURN-CODE.
           PERFORM ERROR-DISPLAY.
           GO TO AAA-EXIT.

       AAA-VALID-RETURN.
           MOVE "A" TO COP-RETURN-CODE.

       AAA-EXIT.
           PERFORM MAB-INPUT-EDIT-RESET.

      * NEED TO SET EXT-TCLP-TRACE-FORM-PATH BEFORE GOING INTO
      * 'CLOSE-FILES' (LIBGB/*DECLARE*) SO THAT IT CAN GET THE EXACT
      * PROGRAM NAME IN THE TRACE LOG
      
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CALCZL"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM CLOSE-FILES.

           EXIT PROGRAM.

      ***********************************
       ABA-CALCULATION  SECTION.
      ***********************************
           PERFORM ACA-CALC-SETUP.
           MOVE CMP-PROCEEDS        TO CPP-PROCEEDS.
           MOVE CMP-REG-PAYMENT     TO CPP-REG-PAYMENT.
           MOVE COP-SCREEN-FIELDS   TO CMP-SCREEN-FIELDS.
           MOVE COP-WORKING-AMOUNTS TO CMP-WORKING-AMOUNTS.
           PERFORM BAA-CALC-ANNUITY-FACTOR.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.
           PERFORM CAA-CALC-PAYMENT-OR-AMT-FINCD.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.
           PERFORM CEA-CALC-1ST-PAYMENT.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.
           PERFORM CGA-CALC-LHINS.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.
           PERFORM CIA-CALC-AHINS.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.
           PERFORM CKA-CALC-PPINS.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.
           PERFORM CLA-CALC-O1-INS.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.

           PERFORM CLA-CALC-O2-INS-TAX.
           PERFORM CLA-CALC-O3-INS-TAX.

           PERFORM CPA-CALC-MAINT-FEES.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.
           PERFORM CSA-CALC-SERVICE-CHARGES.
           IF NOT COP-UNKNOWN-ERROR GO TO ABA-EXIT.
           PERFORM DZA-CLEANUP-UP-FINAL.

       ABA-EXIT.
           EXIT.


      ***********************************
       ACA-CALC-SETUP  SECTION.
      ***********************************
           ADD COP-PROCEEDS COP-GL-AMOUNT-TOTAL
               WKS-PRIOR-LOAN-BALANCE GIVING COP-NEW-LOAN-PROCEEDS.
           PERFORM GET-ALL-INS-PREMIUMS.
           ADD COP-NEW-LOAN-PROCEEDS WKS-ALL-INS-PREMIUMS
                   WKS-ALL-INS-TAX GIVING COP-AMT-FINANCD.
           PERFORM CXF-GET-INS-FINCHG.
           SUBTRACT WKS-INS-FINCHG
                      COP-GL-AMOUNT-TOTAL-FINCHG
              FROM COP-AMT-FINANCD GIVING COP-APR-FINANCD.

           PERFORM ADA-SET-CTBL-INDEX.

       ACA-EXIT.
           EXIT.


      ***********************************
       ADA-SET-CTBL-INDEX  SECTION.
      ***********************************
      * NOTE: CTBL MUST BE INITIALIZED FOR AEA-CAL-INT-CHARGEABLE ROUTINE
           SET CTBL TO 1.
           PERFORM AEA-CALC-INT-CHARGEABLE.
           MOVE COP-INT-CHARGEABLE TO CTBL-INT-CHARGEABLE.
           MOVE COP-TERM TO CTBL-TERM.
           PERFORM SETCTBL.
           MOVE CTBL-INDEX TO CTBL.

           PERFORM AEA-CALC-INT-CHARGEABLE.

       ADA-EXIT.
      * SET PER DIEM DAYS FOR SIMPLE CALCULATIONS:
           MOVE 360 TO WKS-INTMETHOD-DAYS.
           IF SP-CAL-INTMETHOD-DIS(CTBL)
              IF SP-INTMETHOD-EXACT
                 MOVE 365 TO WKS-INTMETHOD-DAYS.

      ***********************************
       AEA-CALC-INT-CHARGEABLE  SECTION.
      ***********************************
           MOVE COP-NEW-LOAN-PROCEEDS TO COP-INT-CHARGEABLE.
           IF SP-CAL-PRO-INS-SC(CTBL) OR
              SP-CAL-PRO-INS(CTBL)
               PERFORM GET-ALL-INS-PREMIUMS
               ADD WKS-ALL-INS-PREMIUMS WKS-ALL-INS-TAX
                                  TO COP-INT-CHARGEABLE.

           IF SP-CAL-PRO-INS-SC(CTBL) OR
              SP-CAL-PRO-SC(CTBL)
              ADD COP-SERVICE-CHG TO COP-INT-CHARGEABLE.

       AEA-EXIT.
           EXIT.


      ***********************************
       BAA-CALC-ANNUITY-FACTOR  SECTION.
      ***********************************
           IF COP-OVR-RATE-IS-NULL
              MOVE 9999.9999 TO COP-OVR-RATE
           ELSE
      *<LOW RATE>
           IF COP-OVR-RATE < 0.011
              MOVE COP-OVR-RATE TO WKS-RATE
              MOVE COP-TERM TO WKS-ANNUITY
                               WKS-INS-ANNUITY
              GO TO BAA-EXIT
           ELSE
           IF COP-OVR-RATE-IS-APR
              PERFORM BAL-APR-OVERIDE
              IF COP-UNKNOWN-ERROR
                 GO TO BAA-SIMPLE-RATES
              ELSE
                 GO TO BAA-EXIT
           ELSE
           IF COP-OVR-RATE-IS-EFF
              IF COP-TERM = 1 AND SP-CAL-SIMPLE(CTBL)
                 PERFORM BAL-APR-OVERIDE
                 IF COP-UNKNOWN-ERROR
                    GO TO BAA-SIMPLE-RATES
                 ELSE
                    GO TO BAA-EXIT
              ELSE
                 MOVE COP-OVR-RATE TO WKS-RATE
           ELSE
              GO TO BAA-ERROR-EXIT.

           IF SP-CAL-ADDON(CTBL)
              GO TO BAA-ADDON-RATES
           ELSE
           IF SP-CAL-SIMPLE(CTBL)
              GO TO BAA-SIMPLE-RATES
           ELSE
           IF SP-CAL-FLAT-MONTHLY(CTBL)
              GO TO BAA-FLAT-MONTHLY.

       BAA-DISCOUNT-RATES.
           IF COP-OVR-RATE-IS-EFF
              PERFORM BBA-1-DISCOUNT
              GO TO BAA-CHECK-FOR-EXTENSION.

           IF SP-CAL-BREAK-ON-TERM(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 MOVE " " TO BLENDED-ADDON-DISC-FG
                 MOVE SP-CAL-RATE(CTBL,1) TO WKS-RATE
                 PERFORM BBA-1-DISCOUNT
              ELSE
                 MOVE "Y" TO BLENDED-ADDON-DISC-FG
                 IF SP-CAL-NORATES(CTBL) = 2
                    PERFORM BBB-2-DISCOUNTS-TERM-BREAK
                 ELSE
                    IF SP-CAL-NORATES(CTBL) = 3
                       PERFORM BBC-3-DISCOUNTS-TERM-BREAK
                    ELSE
                       GO TO BAA-ERROR-EXIT
           ELSE
           IF SP-CAL-BREAK-ON-AMOUNT(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 MOVE " " TO BLENDED-ADDON-DISC-FG
                 MOVE SP-CAL-RATE(CTBL,1) TO WKS-RATE
                 PERFORM BBA-1-DISCOUNT
              ELSE
                 MOVE "Y" TO BLENDED-ADDON-DISC-FG
                 ADD COP-INT-CHARGEABLE COP-INT-CHARGE
                                      GIVING WKS-AMOUNT
                 PERFORM BCG-ADDON-DISC-AMOUNT-BREAK
                 PERFORM BBA-1-DISCOUNT
           ELSE
           IF SP-CAL-BREAK-ON-FLATAMT(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 MOVE " " TO BLENDED-ADDON-DISC-FG
                 MOVE SP-CAL-RATE(CTBL,1) TO WKS-RATE
                 PERFORM BBA-1-DISCOUNT
              ELSE
                 MOVE " " TO BLENDED-ADDON-DISC-FG
                 ADD COP-INT-CHARGEABLE COP-INT-CHARGE
                                      GIVING WKS-AMOUNT
                 PERFORM BCG-FLATAMT-BREAK
                 PERFORM BBA-1-DISCOUNT
           ELSE
              GO TO BAA-ERROR-EXIT.

           GO TO BAA-CHECK-FOR-EXTENSION.

       BAA-ADDON-RATES.
           IF COP-OVR-RATE-IS-EFF
              PERFORM BCA-1-ADDON
              GO TO BAA-CHECK-FOR-EXTENSION.

           IF SP-CAL-BREAK-ON-TERM(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 MOVE " " TO BLENDED-ADDON-DISC-FG
                 MOVE SP-CAL-RATE(CTBL,1) TO WKS-RATE
                 PERFORM BCA-1-ADDON
              ELSE
                 MOVE "Y" TO BLENDED-ADDON-DISC-FG
                 IF SP-CAL-NORATES(CTBL) = 2
                    PERFORM BCB-2-ADDONS-TERM-BREAK
                 ELSE
                    IF SP-CAL-NORATES(CTBL) = 3
                       PERFORM BCC-3-ADDONS-TERM-BREAK
                    ELSE
                       GO TO BAA-ERROR-EXIT
           ELSE
           IF SP-CAL-BREAK-ON-AMOUNT(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 MOVE " " TO BLENDED-ADDON-DISC-FG
                 MOVE SP-CAL-RATE(CTBL,1) TO WKS-RATE
                 PERFORM BCA-1-ADDON
              ELSE
                 MOVE "Y" TO BLENDED-ADDON-DISC-FG
                 MOVE COP-INT-CHARGEABLE TO WKS-AMOUNT
                 PERFORM BCG-ADDON-DISC-AMOUNT-BREAK
                 PERFORM BCA-1-ADDON
           ELSE
           IF SP-CAL-BREAK-ON-FLATAMT(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 MOVE " " TO BLENDED-ADDON-DISC-FG
                 MOVE SP-CAL-RATE(CTBL,1) TO WKS-RATE
                 PERFORM BCA-1-ADDON
              ELSE
                 MOVE " " TO BLENDED-ADDON-DISC-FG
                 MOVE COP-INT-CHARGEABLE TO WKS-AMOUNT
                 PERFORM BCG-FLATAMT-BREAK
                 PERFORM BCA-1-ADDON
           ELSE
              GO TO BAA-ERROR-EXIT.

           GO TO BAA-CHECK-FOR-EXTENSION.

       BAA-SIMPLE-RATES.
           IF COP-OVR-RATE-IS-EFF OR COP-OVR-RATE-IS-APR
              PERFORM BDA-1-SIMPLE
              GO TO BAA-CHECK-FOR-EXTENSION.

           IF SP-CAL-BREAK-ON-TERM(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 MOVE SP-CAL-RATE(CTBL,1) TO WKS-RATE
                 PERFORM BDA-1-SIMPLE
              ELSE
                 IF SP-CAL-NORATES(CTBL) = 2
                    MOVE "K" TO COP-RETURN-CODE
                 ELSE
                    IF SP-CAL-NORATES(CTBL) = 3
                       MOVE "K" TO COP-RETURN-CODE
                    ELSE
                       GO TO BAA-ERROR-EXIT
           ELSE
           IF SP-CAL-BREAK-ON-AMOUNT(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 COMPUTE WKS-RATE ROUNDED = SP-CAL-RATE(CTBL,1)
                                           * 360 / WKS-INTMETHOD-DAYS
                 PERFORM BDA-1-SIMPLE
              ELSE
                 IF SP-CAL-NORATES(CTBL) NOT > 5
                    PERFORM BDF-MANY-SIMPLES-AMOUNT-BREAK
                 ELSE
                    GO TO BAA-ERROR-EXIT
           ELSE
           IF SP-CAL-BREAK-ON-FLATAMT(CTBL)
              IF SP-CAL-NORATES(CTBL) = 1
                 MOVE SP-CAL-RATE(CTBL 1) TO WKS-RATE
              ELSE
                 MOVE COP-INT-CHARGEABLE TO WKS-AMOUNT
                 PERFORM BCG-FLATAMT-BREAK
              END-IF
              PERFORM BDA-1-SIMPLE
           ELSE
              GO TO BAA-ERROR-EXIT.
           GO TO BAA-CHECK-FOR-EXTENSION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BAA-FLAT-MONTHLY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       BAA-FLAT-MONTHLY.
           IF COP-OVR-RATE-IS-EFF
      *       PERFORM BBA-1-DISCOUNT
              GO TO BAA-CHECK-FOR-EXTENSION.

           MOVE COP-INT-CHARGEABLE TO WKS-PRINCIPAL.

           MOVE 0 TO WKS-CHARGE-PER-MONTH.
      *   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *   FIND BREAK WHERE WKS-PRINCIPAL IS <= SP-CAL-BREAK() IN TABLE #1
      *   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO CTBL.
           PERFORM VARYING CSTP FROM 1 BY 1 UNTIL CSTP > 5
              IF SP-CAL-BREAK(CTBL CSTP) = 0
                 GO TO BAA-ERROR-EXIT
              END-IF
              IF WKS-PRINCIPAL <= SP-CAL-BREAK(CTBL CSTP)
                 MOVE SP-CAL-RATE(CTBL CSTP) TO WKS-CHARGE-PER-MONTH
                 MOVE 8 TO CSTP
              END-IF
           END-PERFORM.
           IF CSTP NOT = 9
      *      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *      FIND BREAK WHERE WKS-PRINCIPAL IS <= SP-CAL-BREAK() IN TABLE #2
      *      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              MOVE 2 TO CTBL
              PERFORM VARYING CSTP FROM 1 BY 1 UNTIL CSTP > 5
                 IF SP-CAL-BREAK(CTBL CSTP) = 0
                    GO TO BAA-ERROR-EXIT
                 END-IF
                 IF WKS-PRINCIPAL <= SP-CAL-BREAK(CTBL CSTP)
                    MOVE SP-CAL-RATE(CTBL CSTP) TO WKS-CHARGE-PER-MONTH
                    MOVE 8 TO CSTP
                 END-IF
              END-PERFORM
              IF CSTP NOT = 9
                 GO TO BAA-ERROR-EXIT.

           IF SP-PYEXFRMLA-REG-INCR
              COMPUTE COP-INT-CHARGE ROUNDED =
                  WKS-CHARGE-PER-MONTH
                         * (COP-TERM + INT-MTHS + INT-DAYS/30)
           ELSE
              COMPUTE COP-INT-CHARGE ROUNDED =
                  WKS-CHARGE-PER-MONTH * COP-TERM.

           COMPUTE WKS-ANNUITY ROUNDED =
               WKS-PRINCIPAL
                     / ( (WKS-PRINCIPAL + COP-INT-CHARGE) / COP-TERM ).

           GO TO BAA-EXIT.

       BAA-ERROR-EXIT.
           MOVE "B" TO COP-RETURN-CODE.
           GO TO BAA-EXIT.

       BAA-CHECK-FOR-EXTENSION.
           MOVE WKS-ANNUITY TO WKS-INS-ANNUITY.
           IF NOT COP-UNKNOWN-ERROR
              GO TO BAA-EXIT.
           IF SP-PYEXFRMLA-REG-INCR
              IF NOT (INT-MTHS = 0 AND INT-DAYS = 0)
                 MOVE WKS-ANNUITY TO WKS-BAH-ANNUITY
                 MOVE WKS-RATE TO WKS-BAH-RATE
                 PERFORM BAH-ALL-PAYMENT-EXTENSION
                 MOVE WKS-BAH-ANNUITY TO WKS-ANNUITY.

       BAA-EXIT.
           EXIT.

      ***********************************
       BAH-ALL-PAYMENT-EXTENSION  SECTION.
      ***********************************
           MOVE INT-DAYS TO WKS-TERM-1.
      *    IF COP-TERM = 1 AND SP-DISFRMLA-SPAY-FG = "Y"
      *       PERFORM BAB-ADJUST-FOR-YEARTYPE
      *       ADD WKS-YRTYPE-ADJUST TO WKS-TERM-1.

           IF COP-TERM = 1 OR SP-DISFRMLA-USRULE
              COMPUTE WKS-BAH-ANNUITY ROUNDED =
                 WKS-BAH-ANNUITY * (1 + WKS-BAH-RATE / 1200)
                    / (1 + WKS-BAH-RATE / 1200
                            * (INT-MTHS + 1 + WKS-TERM-1 / 30))
                 SIZE ERROR MOVE "E" TO COP-RETURN-CODE
           ELSE
              COMPUTE WKS-BAH-ANNUITY ROUNDED =
                 WKS-BAH-ANNUITY
                  / ((1 + WKS-BAH-RATE / 1200) ** INT-MTHS
                  * (1 + WKS-BAH-RATE / 36000 * WKS-TERM-1))
                 SIZE ERROR MOVE "E" TO COP-RETURN-CODE.

       BAH-EXIT.
           EXIT.


      ***********************************
       BAL-APR-OVERIDE  SECTION.
      ***********************************
           IF COP-REG-PAYMENT = 0
              MOVE COP-OVR-RATE TO WKS-RATE
              GO TO BAL-EXIT.

           MOVE COP-OVR-RATE TO WKS-RATE2.

           IF NOT (COP-TERM = 1)
              GO TO BAL-NOT-SINGLE-PAY.

           MOVE COP-IN-DT  TO NUM-DATE.
           MOVE COP-PMT-DT TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

           COMPUTE APR-FINANCED ROUNDED =
               (
                 (COP-REG-PAYMENT + COP-1ST-PY-EXT)
                        / ( (WKS-RATE2 * ELAPSED-DAYS / 36500) + 1 )
               )
              SIZE ERROR GO TO BAL-ERROR-EXIT.
           GO TO BAL-GET-RATE.

       BAL-NOT-SINGLE-PAY.
           IF INT-MTHS = 0 AND INT-DAYS = 0
              MOVE 1 TO WKS-94V913
           ELSE
              IF SP-DISFRMLA-USRULE
                 COMPUTE WKS-94V913 ROUNDED =
                    (1 + WKS-RATE2 / 1200)
                       / (
                          1 + WKS-RATE2 / 1200
                             * (INT-MTHS + 1 + INT-DAYS / 30)
                         )
                    SIZE ERROR GO TO BAL-ERROR-EXIT
              ELSE
                 COMPUTE WKS-94V913 ROUNDED =
                    1 / (
                         (1 + WKS-RATE2 / 1200) ** INT-MTHS
                              * (1 + WKS-RATE2 / 36000 * INT-DAYS)
                        )
                    SIZE ERROR GO TO BAL-ERROR-EXIT.

           COMPUTE APR-FINANCED ROUNDED = (COP-REG-PAYMENT
              * 1200 * (1 - 1 / (1 + WKS-RATE2 / 1200) ** COP-TERM)
              / WKS-RATE2 + COP-1ST-PY-EXT
              / (1 + WKS-RATE2 / 1200)) * WKS-94V913
              SIZE ERROR GO TO BAL-ERROR-EXIT.

       BAL-GET-RATE.
           PERFORM GET-ALL-INS-PREMIUMS.
           COMPUTE APR-FINANCED ROUNDED = APR-FINANCED
                - WKS-ALL-INS-PREMIUMS - WKS-ALL-INS-TAX.

           IF APR-FINANCED NOT POSITIVE
              MOVE COP-OVR-RATE TO WKS-RATE
              GO TO BAL-EXIT.
           IF COP-TOTAL-NOTE = 0
              MULTIPLY COP-REG-PAYMENT BY COP-TERM GIVING WKS-REGPYAMT
           ELSE
              MOVE COP-TOTAL-NOTE TO WKS-REGPYAMT.
           IF SP-CAL-PRO-INS(CTBL) OR SP-CAL-PRO-INS-SC(CTBL)
              ADD WKS-ALL-INS-PREMIUMS WKS-ALL-INS-TAX
                                       TO APR-FINANCED
           ELSE
              SUBTRACT WKS-ALL-INS-PREMIUMS WKS-ALL-INS-TAX
                                          FROM WKS-REGPYAMT.
           IF SP-CAL-PRO-SC(CTBL) OR SP-CAL-PRO-INS-SC(CTBL)
              ADD COP-SERVICE-CHG TO APR-FINANCED
           ELSE
              SUBTRACT COP-SERVICE-CHG FROM WKS-REGPYAMT.
           IF WKS-REGPYAMT NOT POSITIVE
              GO TO BAL-ERROR-EXIT.
           DIVIDE WKS-REGPYAMT BY COP-TERM GIVING APR-REGPYAMT.
           COMPUTE WKS-TEST-AMOUNT = COP-TERM * APR-REGPYAMT.
           IF NOT (WKS-REGPYAMT = WKS-TEST-AMOUNT)
              ADD .01 TO APR-REGPYAMT.
           IF COP-ODD-1ST-PMT = 0
              MOVE COP-TERM TO APR-NOREGPY
              MOVE 0        TO APR-NOODDPY
                               APR-1STPYAMT
           ELSE
              SUBTRACT 1 FROM COP-TERM GIVING APR-NOREGPY
              MOVE 1 TO APR-NOODDPY
              MOVE COP-ODD-1ST-PMT TO APR-1STPYAMT.
           MOVE COP-IN-DT  TO APR-INTDATE.
           MOVE COP-PMT-DT TO APR-1STPYDATE.
           MOVE SP-DISFRMLA TO WKS-DISFRMLA.
           IF NOT SP-PYEXFRMLA-REG-INCR
              MOVE "B" TO SP-DISFRMLA.

           PERFORM ZCA-APR-REGZ.

           MOVE WKS-DISFRMLA TO SP-DISFRMLA.
           IF APR-APR NOT = 0
              MOVE APR-APR TO WKS-RATE
           ELSE
              MOVE WKS-RATE2 TO WKS-RATE.
           GO TO BAL-EXIT.

       BAL-ERROR-EXIT.
           MOVE "N" TO COP-RETURN-CODE.

       BAL-EXIT.
           MOVE WKS-RATE TO WKS-HOLD-EFFRATE.


      ***********************************
       BBA-1-DISCOUNT  SECTION.
      ***********************************
           COMPUTE WKS-ANNUITY ROUNDED =
             (1 - WKS-RATE * COP-TERM / 1200) * COP-TERM
               SIZE ERROR
                 MOVE WKS-ALL-NINES TO WKS-ANNUITY
                 GO TO BBA-ERROR-EXIT.

           IF WKS-ANNUITY POSITIVE
              GO TO BBA-EXIT.
           MOVE 0 TO WKS-ANNUITY.

       BBA-ERROR-EXIT.
           MOVE "K" TO COP-RETURN-CODE.

       BBA-EXIT.
           EXIT.


      ***********************************
       BBB-2-DISCOUNTS-TERM-BREAK  SECTION.
      ***********************************
           IF COP-TERM NOT > SP-CAL-BREAK(CTBL,1)
              MOVE COP-TERM TO WKS-TERM-1
              MOVE 0 TO WKS-TERM-2
           ELSE
              MOVE SP-CAL-BREAK(CTBL,1) TO WKS-TERM-1
              SUBTRACT SP-CAL-BREAK(CTBL,1) FROM COP-TERM
                GIVING WKS-TERM-2.

           COMPUTE WKS-RATE ROUNDED =
                 (WKS-TERM-1 * SP-CAL-RATE(CTBL,1) + WKS-TERM-2 *
                  SP-CAL-RATE(CTBL,2)) / COP-TERM
               SIZE ERROR
                 MOVE WKS-ALL-NINES TO WKS-RATE
                 MOVE "K" TO COP-RETURN-CODE
                 GO TO BBB-EXIT.

           PERFORM BBA-1-DISCOUNT.

       BBB-EXIT.
           EXIT.


      ***********************************
       BBC-3-DISCOUNTS-TERM-BREAK  SECTION.
      ***********************************
           IF COP-TERM NOT > SP-CAL-BREAK(CTBL,1)
              MOVE COP-TERM TO WKS-TERM-1
              MOVE 0        TO WKS-TERM-2
                               WKS-TERM-3
           ELSE
              IF COP-TERM NOT > SP-CAL-BREAK(CTBL,2)
                 MOVE SP-CAL-BREAK(CTBL,1) TO WKS-TERM-1
                 SUBTRACT SP-CAL-BREAK(CTBL,1) FROM COP-TERM
                   GIVING WKS-TERM-2
                 MOVE 0 TO WKS-TERM-3
              ELSE
                 MOVE SP-CAL-BREAK(CTBL,1) TO WKS-TERM-1
                 SUBTRACT SP-CAL-BREAK(CTBL,1)
                    FROM SP-CAL-BREAK(CTBL,2) GIVING WKS-TERM-2
                 SUBTRACT SP-CAL-BREAK(CTBL,2) FROM COP-TERM
                    GIVING WKS-TERM-3.

           COMPUTE WKS-RATE ROUNDED =
                 (WKS-TERM-1 * SP-CAL-RATE(CTBL,1)
                + WKS-TERM-2 * SP-CAL-RATE(CTBL,2)
                + WKS-TERM-3 * SP-CAL-RATE(CTBL,3)) / COP-TERM
               SIZE ERROR
                 MOVE WKS-ALL-NINES TO WKS-RATE
                 MOVE "K" TO COP-RETURN-CODE
                 GO TO BBC-EXIT.

           PERFORM BBA-1-DISCOUNT.

       BBC-EXIT.
           EXIT.

      ***********************************
       BCA-1-ADDON  SECTION.
      ***********************************
           COMPUTE WKS-ANNUITY ROUNDED =
             COP-TERM / (1 + WKS-RATE * COP-TERM / 1200)
               SIZE ERROR
                 MOVE WKS-ALL-NINES TO WKS-ANNUITY
                 GO TO BCA-ERROR-EXIT.

           IF WKS-ANNUITY POSITIVE
              GO TO BCA-EXIT.
           MOVE 0 TO WKS-ANNUITY.

       BCA-ERROR-EXIT.
           MOVE "K" TO COP-RETURN-CODE.

       BCA-EXIT.
           EXIT.


      ***********************************
       BCB-2-ADDONS-TERM-BREAK  SECTION.
      ***********************************
           IF COP-TERM NOT > SP-CAL-BREAK(CTBL,1)
              MOVE COP-TERM TO WKS-TERM-1
              MOVE 0 TO WKS-TERM-2
           ELSE
              MOVE SP-CAL-BREAK(CTBL,1) TO WKS-TERM-1
              SUBTRACT SP-CAL-BREAK(CTBL,1) FROM COP-TERM
                 GIVING WKS-TERM-2.

           COMPUTE WKS-RATE ROUNDED =
                 (WKS-TERM-1 * SP-CAL-RATE(CTBL,1) + WKS-TERM-2 *
                  SP-CAL-RATE(CTBL,2)) / COP-TERM
               SIZE ERROR
                 MOVE WKS-ALL-NINES TO WKS-RATE
                 MOVE "K" TO COP-RETURN-CODE
                 GO TO BCB-EXIT.

           PERFORM BCA-1-ADDON.

       BCB-EXIT.
           EXIT.


      ***********************************
       BCC-3-ADDONS-TERM-BREAK  SECTION.
      ***********************************
           IF COP-TERM NOT > SP-CAL-BREAK(CTBL,1)
              MOVE COP-TERM TO WKS-TERM-1
              MOVE 0        TO WKS-TERM-2
              MOVE 0        TO WKS-TERM-3
           ELSE
              IF COP-TERM NOT > SP-CAL-BREAK(CTBL,2)
                 MOVE SP-CAL-BREAK(CTBL,1) TO WKS-TERM-1
                 SUBTRACT SP-CAL-BREAK(CTBL,1) FROM COP-TERM
                   GIVING WKS-TERM-2
                 MOVE 0 TO WKS-TERM-3
              ELSE
                 MOVE SP-CAL-BREAK(CTBL,1) TO WKS-TERM-1
                 SUBTRACT SP-CAL-BREAK(CTBL,1)
                   FROM SP-CAL-BREAK(CTBL,2) GIVING WKS-TERM-2
                 SUBTRACT SP-CAL-BREAK(CTBL,2) FROM COP-TERM
                   GIVING WKS-TERM-3.

           COMPUTE WKS-RATE ROUNDED =
                 (WKS-TERM-1 * SP-CAL-RATE(CTBL,1)
                + WKS-TERM-2 * SP-CAL-RATE(CTBL,2)
                + WKS-TERM-3 * SP-CAL-RATE(CTBL,3)) / COP-TERM
               SIZE ERROR
                 MOVE WKS-ALL-NINES TO WKS-RATE
                 MOVE "K" TO COP-RETURN-CODE
                 GO TO BCC-EXIT.

           PERFORM BCA-1-ADDON.

       BCC-EXIT.
           EXIT.

      ***********************************
       BCG-ADDON-DISC-AMOUNT-BREAK SECTION.
      ***********************************
           SET CSTP TO 0.
           MOVE 0 TO WKS-AMOUNT-2.
           MOVE WKS-AMOUNT TO WKS-AMOUNT-3.

       BCG-ADDON-DISC-AMOUNT-LOOP.
           SET CSTP UP BY 1.
           IF CSTP > SP-CAL-NORATES(CTBL)
              GO TO BCG-ADDON-DISC-AMOUNT-FINAL.
           IF SP-CAL-BREAK(CTBL CSTP) = 0
              MOVE WKS-AMOUNT TO WKS-AMOUNT-1
           ELSE
              IF CSTP = 1
                 MOVE SP-CAL-BREAK(CTBL CSTP) TO WKS-AMOUNT-1
              ELSE
                 SUBTRACT SP-CAL-BREAK(CTBL CSTP - 1) FROM
                             SP-CAL-BREAK(CTBL CSTP)
                                       GIVING WKS-AMOUNT-1.
           IF WKS-AMOUNT > WKS-AMOUNT-1
              SUBTRACT WKS-AMOUNT-1 FROM WKS-AMOUNT
           ELSE
              MOVE WKS-AMOUNT TO WKS-AMOUNT-1
              MOVE 0 TO WKS-AMOUNT.
           COMPUTE WKS-AMOUNT-2 ROUNDED = WKS-AMOUNT-2 +
                          SP-CAL-RATE(CTBL CSTP) * WKS-AMOUNT-1.
           GO TO BCG-ADDON-DISC-AMOUNT-LOOP.

       BCG-ADDON-DISC-AMOUNT-FINAL.
           COMPUTE WKS-RATE ROUNDED =
              WKS-AMOUNT-2 / WKS-AMOUNT-3
              SIZE ERROR
                MOVE WKS-ALL-NINES TO WKS-RATE
                MOVE "K" TO COP-RETURN-CODE
                GO TO BCG-ADDON-DISC-AMOUNT-EXIT.

       BCG-ADDON-DISC-AMOUNT-EXIT.
           EXIT.

      ******************************************************************
      *    SIMPLE/ADDON/DISCOUNT FLAT AMOUNT BREAK ROUTINE
      *
      *    THIS ROUTINE FINDS THE RATE FOR WKS-AMOUNT SUCH THAT
      *    WKS-AMOUNT IS LESS THAT OR EQUAL TO THE AMOUNT BREAK
      *    IN THE TABLE
      ******************************************************************
       BCG-FLATAMT-BREAK SECTION.
           SET CSTP TO 0.

       BCG-FLATAMT-LOOP.
           SET CSTP UP BY 1.
           IF CSTP > SP-CAL-NORATES(CTBL)
              MOVE "K" TO COP-RETURN-CODE
           ELSE
              IF SP-CAL-BREAK(CTBL CSTP) = 0
                     OR WKS-AMOUNT <= SP-CAL-BREAK(CTBL CSTP)
                 MOVE SP-CAL-RATE(CTBL CSTP) TO WKS-RATE
              ELSE
                 GO TO BCG-FLATAMT-LOOP.

       BCG-FLATAMT-EXIT.
           EXIT.

      ***********************************
       BDA-1-SIMPLE  SECTION.
      ***********************************
           COMPUTE WKS-ANNUITY ROUNDED = 1200 * (1 - 1 /
             (1 + WKS-RATE / 1200) ** COP-TERM) / WKS-RATE
               SIZE ERROR
                 MOVE 9999.9999 TO WKS-ANNUITY
                 GO TO BDA-ERROR-EXIT.

           IF WKS-ANNUITY POSITIVE
              GO TO BDA-EXIT.

           MOVE 0 TO WKS-ANNUITY.

       BDA-ERROR-EXIT.
           MOVE "K" TO COP-RETURN-CODE.

       BDA-EXIT.
           EXIT.


      *********************************************
       BDF-MANY-SIMPLES-AMOUNT-BREAK  SECTION.
      *********************************************
           MOVE COP-TERM TO MIR-TERM.
           MULTIPLY COP-INT-CHARGEABLE BY 100
                                  GIVING MIR-INT-CHARGEABLE.
           MULTIPLY COP-REG-PAYMENT BY 100
                                  GIVING MIR-REG-PAYMENT.
           MOVE SP-CAL-NORATES(CTBL) TO MIR-NO-RATES.
           PERFORM BDF-MOVE-IN-RATES
              VARYING CSTP FROM 1 BY 1
              UNTIL CSTP > SP-CAL-NORATES(CTBL).
           GO TO BDF-CALL-THE-CALC-PROGRAM.
       BDF-MOVE-IN-RATES.
           SET MIRX TO CSTP.
           COMPUTE MIR-RATE(MIRX) ROUNDED =
             SP-CAL-RATE(CTBL,CSTP) * 3600000 / WKS-INTMETHOD-DAYS.
           MULTIPLY SP-CAL-BREAK(CTBL,CSTP) BY 100
             GIVING MIR-BREAK(MIRX).

       BDF-CALL-THE-CALC-PROGRAM.
           IF NOT CIP-REG-PAYMENT-IS-INPUT
              MOVE 0 TO MIR-REG-PAYMENT.
           PERFORM BDI-ANOTHER-LANGUAGE.
           MOVE MIR-RETURN-CODE TO COP-RETURN-CODE.
           IF NOT COP-UNKNOWN-ERROR
             GO TO BDF-EXIT.

           MOVE SP-DISFRMLA TO WKS-DISFRMLA.
           MOVE "B" TO SP-DISFRMLA.
           MULTIPLY MIR-BALANCE BY .01 GIVING APR-FINANCED.
           MULTIPLY MIR-PAYMENT BY .01 GIVING APR-REGPYAMT.
           MOVE COP-TERM TO APR-NOREGPY.
           MOVE 0 TO APR-NOODDPY APR-1STPYAMT APR-EXTCHG.
           MOVE COP-IN-DT TO APR-INTDATE.
           MOVE COP-PMT-DT TO APR-1STPYDATE.
           PERFORM ZCA-APR-REGZ.
           MOVE WKS-DISFRMLA TO SP-DISFRMLA.

      *  JTG 011102 CORRECTED DOUBLE DIP OF 365 FACTOR IN
      *             BDF-MANY-SIMPLES-AMOUNT-BREAK ROUTINE     PIKCO
      *    IF COP-TERM = 1
      *       MOVE APR-APR TO WKS-RATE
      *    ELSE
      *       COMPUTE WKS-RATE ROUNDED = APR-APR
      *                          * 360 / WKS-INTMETHOD-DAYS.

           MOVE APR-APR TO WKS-RATE.
           PERFORM BDA-1-SIMPLE.

       BDF-EXIT.
           EXIT.


      ********************************************
      *    CALL MULSIML MANY RATE ROUTINES
      ********************************************
       BDI-ANOTHER-LANGUAGE  SECTION.
           MOVE "11" TO MIR-STAT.
           MOVE "LP/MULSIL" TO FORM-NAM.
           CALL FORM-PROGX USING MIR-STAT
                                MIR-MANY-INTEREST-RATES-SIMPLE.
           CANCEL FORM-PROGX.

       BDI-EXIT.
           EXIT.


      *********************************************
       CAA-CALC-PAYMENT-OR-AMT-FINCD  SECTION.
      *********************************************
           IF COP-REG-PAYMENT-IS-INPUT
              PERFORM CAK-CALC-NOTE-UNPAID-AND-PRO
              GO TO CAA-EXIT.

      *------------------------------------------------------------
      *    PAYMENT ROUND OPTIONS:
      *      S - ROUND WHEN SPR RATE IS NOT OVERRIDEN (DEFAULT)
      *      R - ROUND IN ALL CASES
      *      N - NEVER ROUND
      *      O - ROUND WHEN SPR RATES ARE OVERRIDEN
      *------------------------------------------------------------
           IF SP-CAL-PAYMENT-ROUND-FG = "R"
                  OR (SP-CAL-PAYMENT-ROUND-FG = "S"
                                AND COP-OVR-RATE-IS-NULL)
                  OR (SP-CAL-PAYMENT-ROUND-FG = "O"
                                AND NOT COP-OVR-RATE-IS-NULL)
                  OR (COP-OVR-RATE-IS-NULL AND
                           (NOT (SP-CAL-PAYMENT-ROUND-FG = "S" OR "R"
                                                        OR "N" OR "O"))
                     )
              COMPUTE WKS-REG-PAYMENT ROUNDED =
                 COP-INT-CHARGEABLE / WKS-ANNUITY
                 SIZE ERROR MOVE WKS-ALL-NINES TO WKS-REG-PAYMENT
                            MOVE "D" TO COP-RETURN-CODE
                            GO TO CAA-EXIT
           ELSE
              COMPUTE WKS-REG-PAYMENT =
                 COP-INT-CHARGEABLE / WKS-ANNUITY
                 SIZE ERROR MOVE WKS-ALL-NINES TO WKS-REG-PAYMENT
                            MOVE "D" TO COP-RETURN-CODE
                            GO TO CAA-EXIT.

           MOVE WKS-REG-PAYMENT TO WKS-REGPYAMT.
           PERFORM CAF-CALC-TOTAL-NOTE.

           IF SP-CAL-FLAT-MONTHLY(CTBL)
              IF COP-RETURN-CODE NOT = "D"
                 PERFORM GET-ALL-INS-PREMIUMS
                 SUBTRACT COP-NEW-LOAN-PROCEEDS COP-SERVICE-CHG
                          WKS-ALL-INS-PREMIUMS
                          WKS-ALL-INS-TAX FROM COP-TOTAL-NOTE
                          GIVING WKS-98V92
                 IF WKS-98V92 >= COP-INT-CHARGE
                    IF WKS-REGPYAMT > 0.01
                       SUBTRACT 0.01 FROM WKS-REGPYAMT
                                     GIVING WKS-REG-PAYMENT
                       PERFORM CAF-CALC-TOTAL-NOTE
                    ELSE
                       MOVE "D" TO COP-RETURN-CODE.

       CAA-EXIT.
           EXIT.


      *********************************************
       CAF-CALC-TOTAL-NOTE  SECTION.
      *********************************************
           PERFORM GET-ALL-INS-PREMIUMS.
           IF WKS-REG-PAYMENT NOT POSITIVE
              MOVE 0 TO COP-REG-PAYMENT
              MOVE "D" TO COP-RETURN-CODE
              GO TO CAF-EXIT.

           IF NOT SP-CAL-PRO-INS-SC(CTBL)
              GO TO CAF-TEST-FOR-PRO-INS.
           MULTIPLY WKS-REG-PAYMENT BY COP-TERM
                                 GIVING COP-TOTAL-NOTE
              SIZE ERROR GO TO CAF-ERROR-EXIT.
           GO TO CAF-SET-COP-REG-PAYMENT.

       CAF-TEST-FOR-PRO-INS.
           IF NOT SP-CAL-PRO-INS(CTBL)
              GO TO CAF-TEST-FOR-PRO-SC.
           COMPUTE COP-TOTAL-NOTE = WKS-REG-PAYMENT * COP-TERM
                                                + COP-SERVICE-CHG
              SIZE ERROR GO TO CAF-ERROR-EXIT.
           GO TO CAF-REROUND-WKS-PAYMENT.

       CAF-TEST-FOR-PRO-SC.
           IF NOT SP-CAL-PRO-SC(CTBL)
              GO TO CAF-HANDLE-PROCEEDS.
           COMPUTE COP-TOTAL-NOTE = WKS-REG-PAYMENT * COP-TERM
              + WKS-ALL-INS-PREMIUMS + WKS-ALL-INS-TAX
              SIZE ERROR GO TO CAF-ERROR-EXIT.
           GO TO CAF-REROUND-WKS-PAYMENT.

       CAF-HANDLE-PROCEEDS.
           COMPUTE COP-TOTAL-NOTE = WKS-REG-PAYMENT * COP-TERM
              + WKS-ALL-INS-PREMIUMS + WKS-ALL-INS-TAX
              + COP-SERVICE-CHG
              SIZE ERROR GO TO CAF-ERROR-EXIT.

       CAF-REROUND-WKS-PAYMENT.
           IF BLENDED-ADDON-DISC-FG = "Y"
              DIVIDE COP-TERM INTO COP-TOTAL-NOTE
                                   GIVING WKS-REG-PAYMENT
           ELSE
              DIVIDE COP-TERM INTO COP-TOTAL-NOTE
                                   GIVING WKS-REG-PAYMENT ROUNDED.

           MULTIPLY COP-TERM BY WKS-REG-PAYMENT
                                      GIVING COP-TOTAL-NOTE.

       CAF-SET-COP-REG-PAYMENT.
           ADD WKS-MTHLY-MAINT-FEES TO WKS-REG-PAYMENT.
           MOVE WKS-REG-PAYMENT TO COP-REG-PAYMENT.
           ADD COP-TOTAL-NOTE COP-MAINT-FEES COP-1ST-PY-EXT
                                           GIVING COP-TOT-REPYBLE.
           GO TO CAF-EXIT.

       CAF-ERROR-EXIT.
           MOVE "D" TO COP-RETURN-CODE.
           MOVE WKS-ALL-NINES TO COP-REG-PAYMENT.

       CAF-EXIT.
           EXIT.


      *********************************************
       CAK-CALC-NOTE-UNPAID-AND-PRO  SECTION.
      *********************************************
           PERFORM GET-ALL-INS-PREMIUMS.
           MOVE COP-REG-PAYMENT TO WKS-REG-PAYMENT.
           COMPUTE COP-TOT-REPYBLE = COP-REG-PAYMENT * COP-TERM
                                                  + COP-1ST-PY-EXT
              SIZE ERROR MOVE WKS-ALL-NINES TO COP-TOT-REPYBLE
                         MOVE "D" TO COP-RETURN-CODE
                         GO TO CAK-EXIT.

           SUBTRACT COP-1ST-PY-EXT COP-MAINT-FEES
                         FROM COP-TOT-REPYBLE GIVING COP-TOTAL-NOTE
              SIZE ERROR MOVE WKS-ALL-NINES TO COP-TOTAL-NOTE
                         MOVE "D" TO COP-RETURN-CODE
                         GO TO CAK-EXIT.

           MOVE COP-TOTAL-NOTE TO COP-INT-CHARGEABLE.
           IF SP-CAL-PROCEEDS(CTBL)
              SUBTRACT COP-SERVICE-CHG WKS-ALL-INS-PREMIUMS
                  WKS-ALL-INS-TAX
                  FROM COP-INT-CHARGEABLE
              GO TO CAK-GET-PROCEEDS.
           IF SP-CAL-PRO-INS(CTBL)
              SUBTRACT COP-SERVICE-CHG FROM COP-INT-CHARGEABLE
              GO TO CAK-GET-PROCEEDS.
           IF SP-CAL-PRO-SC(CTBL)
              SUBTRACT WKS-ALL-INS-PREMIUMS
                       WKS-ALL-INS-TAX FROM COP-INT-CHARGEABLE.

       CAK-GET-PROCEEDS.
           IF COP-INT-CHARGEABLE NEGATIVE
              GO TO CAK-SWITCH-TO-CALC-PAYMENT.
           COMPUTE COP-INT-CHARGEABLE = COP-INT-CHARGEABLE
                        * WKS-ANNUITY / COP-TERM + .0099999
              SIZE ERROR MOVE WKS-ALL-NINES TO COP-PROCEEDS
                         MOVE "K" TO COP-RETURN-CODE
                         GO TO CAK-EXIT.

           MOVE COP-INT-CHARGEABLE TO COP-NEW-LOAN-PROCEEDS.
           IF SP-CAL-PROCEEDS(CTBL)
              GO TO CAK-FINISH-PROCEEDS.
           IF SP-CAL-PRO-INS(CTBL)
              SUBTRACT WKS-ALL-INS-PREMIUMS WKS-ALL-INS-TAX
                         FROM COP-NEW-LOAN-PROCEEDS
              GO TO CAK-FINISH-PROCEEDS.
           IF SP-CAL-PRO-SC(CTBL)
              SUBTRACT COP-SERVICE-CHG FROM COP-NEW-LOAN-PROCEEDS
              GO TO CAK-FINISH-PROCEEDS.
           SUBTRACT COP-SERVICE-CHG WKS-ALL-INS-PREMIUMS
                    WKS-ALL-INS-TAX FROM COP-NEW-LOAN-PROCEEDS.

       CAK-FINISH-PROCEEDS.
           SUBTRACT COP-GL-AMOUNT-TOTAL WKS-PRIOR-LOAN-BALANCE
                    FROM COP-NEW-LOAN-PROCEEDS GIVING WKS-PROCEEDS.
           IF WKS-PROCEEDS NEGATIVE
              GO TO CAK-SWITCH-TO-CALC-PAYMENT.
           MOVE WKS-PROCEEDS TO COP-PROCEEDS.
           ADD COP-NEW-LOAN-PROCEEDS WKS-ALL-INS-PREMIUMS
               WKS-ALL-INS-TAX GIVING COP-AMT-FINANCD.
           GO TO CAK-EXIT.

       CAK-SWITCH-TO-CALC-PAYMENT.
           MOVE "I" TO COP-PROCEEDS-SW
           MOVE 0   TO COP-PROCEEDS.
           MOVE "C" TO COP-REG-PAYMENT-SW.

       CAK-EXIT.
           EXIT.


      *********************************************
       CEA-CALC-1ST-PAYMENT  SECTION.
      *********************************************
           MOVE 0 TO WKS-ODD-1ST-PMT.

           IF COP-1ST-PY-EXT-IS-INPUT
              ADD COP-1ST-PY-EXT WKS-REG-PAYMENT
                                      GIVING WKS-ODD-1ST-PMT
           ELSE
           IF SP-PYEXFRMLA-NO-EXT
                  OR SP-PYEXFRMLA-REG-INCR
                   OR (COP-1ST-PMT-EXTENSION = 0)
              GO TO CEA-SET-1ST-PAYMENT
           ELSE
           IF SP-PYEXFRMLA-1ST-INCR-A   OR
              SP-PYEXFRMLA-SAME2-SPREAD OR
              SP-PYEXFRMLA-7
      *<LOW RATE>
              IF WKS-RATE < 0.011
                 MOVE WKS-REG-PAYMENT TO WKS-ODD-1ST-PMT
              ELSE
                 PERFORM CEE-PYEXFRMLA-2OR7OR12
              END-IF
           ELSE
           IF SP-PYEXFRMLA-1ST-INCR-B OR SP-PYEXFRMLA-1ST-INCR-C
              PERFORM CEF-OLD-TYPE-B-INCREMENT
           ELSE
           IF SP-PYEXFRMLA-SPREAD-INT OR SP-PYEXFRMLA-11
              PERFORM CEG-CHARGE-INTEREST-SPREAD
           ELSE
           IF SP-PYEXFRMLA-1ST-PRD-78
              PERFORM CEH-1ST-PERIOD-RULE78
           ELSE
           IF SP-PYEXFRMLA-SPLIT-RATE
      *<LOW RATE>
              IF WKS-RATE < 0.011
                 MOVE WKS-REG-PAYMENT TO WKS-ODD-1ST-PMT
              ELSE
                 PERFORM CEI-1STPY-SPLIT-RATE.

       CEA-SET-1ST-PAYMENT.
           MOVE WKS-ODD-1ST-PMT TO COP-ODD-1ST-PMT.
           IF NOT COP-UNKNOWN-ERROR
              GO TO CEA-EXIT.
           IF WKS-ODD-1ST-PMT = WKS-REG-PAYMENT
              MOVE 0 TO WKS-ODD-1ST-PMT COP-ODD-1ST-PMT.
           IF WKS-ODD-1ST-PMT = 0
              MOVE 0 TO COP-1ST-PY-EXT
           ELSE
              SUBTRACT WKS-REG-PAYMENT FROM WKS-ODD-1ST-PMT
                                         GIVING COP-1ST-PY-EXT.
       CEA-EXIT.
           EXIT.


      *********************************************
       CEE-PYEXFRMLA-2OR7OR12  SECTION.
      *********************************************
           IF COP-OVR-RATE-IS-EFF AND SP-CAL-SIMPLE(CTBL)
              MOVE COP-OVR-RATE TO WKS-RATE-CEE
           ELSE
              IF SP-CAL-SIMPLE(CTBL)
                 MOVE WKS-RATE TO WKS-RATE-CEE
              ELSE
                 MOVE SP-DISFRMLA TO WKS-DISFRMLA
                 MOVE "B" TO SP-DISFRMLA
                 PERFORM XBA-CALCULATE-SIMPLE-INTEREST
                 MOVE WKS-DISFRMLA TO SP-DISFRMLA
                 MOVE APR-APR TO WKS-RATE-CEE.

      * SET UP AMOUNT TO TAKE PER DIEM RATE ON:
           MOVE COP-INT-CHARGEABLE TO WKS-WORKER.
           IF SP-PYEXFRMLA-7
              IF SP-CAL-PRO-SC(CTBL) OR SP-CAL-PRO-INS-SC(CTBL)
                 SUBTRACT COP-SERVICE-CHG FROM WKS-WORKER.

           IF SP-INTMETHOD-EXACT
              MOVE 365 TO WKS-WORKER2
           ELSE
              MOVE 360 TO WKS-WORKER2.

           COMPUTE WKS-ODD-1ST-PMT = WKS-REG-PAYMENT
              + WKS-WORKER * COP-1ST-PMT-EXTENSION
                    * WKS-RATE-CEE / (WKS-WORKER2 * 100)
             SIZE ERROR MOVE "E" TO COP-RETURN-CODE.

       CEE-EXIT.
           EXIT.


      *********************************************
       CEF-OLD-TYPE-B-INCREMENT  SECTION.
      *********************************************
           IF COP-TERM NOT > 35
               MOVE .015 TO WKS-RATE-CEF
             ELSE
               MOVE .005 TO WKS-RATE-CEF.
           IF SP-PYEXFRMLA-1ST-INCR-C
              MOVE .015 TO WKS-RATE-CEF.
           COMPUTE WKS-ODD-1ST-PMT =
               WKS-REG-PAYMENT
                 + COP-TOTAL-NOTE
                      * COP-1ST-PMT-EXTENSION * WKS-RATE-CEF / 30
             SIZE ERROR MOVE "E" TO COP-RETURN-CODE.

       CEF-EXIT.
           EXIT.


      *********************************************
       CEG-CHARGE-INTEREST-SPREAD  SECTION.
      *********************************************
           IF SP-PYEXFRMLA-11
              COMPUTE WKS-ODD-1ST-PMT ROUNDED = WKS-REG-PAYMENT +
                               ( COP-INT-CHARGE / COP-TERM  )
               SIZE ERROR MOVE "E" TO COP-RETURN-CODE
           ELSE
              COMPUTE WKS-ODD-1ST-PMT = WKS-REG-PAYMENT
                       + COP-INT-CHARGE
                       / (COP-TERM * 30) * COP-1ST-PMT-EXTENSION
             SIZE ERROR MOVE "E" TO COP-RETURN-CODE.

       CEG-EXIT.
           EXIT.


      *********************************************
       CEH-1ST-PERIOD-RULE78  SECTION.
      *********************************************
      * COMPUTE RULE OF 78TH UNIT CHARGE
           COMPUTE WKS-UNIT-CEH ROUNDED = COP-INT-CHARGE /
                        (COP-TERM * (COP-TERM + 1) / 2).
      * EARNINGS OF 1ST PERIOD IS ALWAYS THE ORIGINAL TERM
           COMPUTE WKS-ODD-1ST-PMT = WKS-REG-PAYMENT
             + (COP-TERM * WKS-UNIT-CEH / 30)
             * COP-1ST-PMT-EXTENSION
             SIZE ERROR MOVE "E" TO COP-RETURN-CODE.

       CEH-EXIT.
           EXIT.

      *********************************************
       CEI-1STPY-SPLIT-RATE   SECTION.
      *********************************************
           MOVE COP-INT-CHARGEABLE TO WKS-WORKER.
           MOVE 0 TO WKS-AMOUNT.
           IF COP-OVR-RATE-IS-EFF AND SP-CAL-SIMPLE(CTBL)
              MOVE COP-OVR-RATE TO WKS-HOLD-EFFRATE.
           IF COP-OVR-RATE-IS-EFF AND SP-CAL-SIMPLE(CTBL)
                        OR COP-OVR-RATE-IS-APR
              COMPUTE WKS-AMOUNT ROUNDED =
                          WKS-WORKER * WKS-HOLD-EFFRATE / 36000
                      SIZE ERROR MOVE "E" TO COP-RETURN-CODE
                      GO TO CEI-DUE-FINAL.
           IF COP-OVR-RATE-IS-EFF AND SP-CAL-SIMPLE(CTBL)
                        OR COP-OVR-RATE-IS-APR
              GO TO CEI-DUE-FINAL.
           SET CSTP TO 0.
       CEI-DUE-LOOP.
           IF WKS-WORKER = 0
              GO TO CEI-DUE-FINAL.
           SET CSTP UP BY 1.
           IF CSTP > SP-CAL-NORATES(CTBL)
              GO TO CEI-DUE-FINAL.
           IF SP-CAL-BREAK(CTBL CSTP) = 0
              MOVE WKS-WORKER TO WKS-AMOUNT-1
           ELSE
              IF CSTP = 1
                 MOVE SP-CAL-BREAK(CTBL CSTP) TO WKS-AMOUNT-1
              ELSE
                 SUBTRACT SP-CAL-BREAK(CTBL CSTP - 1) FROM
                             SP-CAL-BREAK(CTBL CSTP)
                                GIVING WKS-AMOUNT-1.
           IF WKS-WORKER > WKS-AMOUNT-1
              SUBTRACT WKS-AMOUNT-1 FROM WKS-WORKER
           ELSE
              MOVE WKS-WORKER TO WKS-AMOUNT-1
              MOVE 0 TO WKS-WORKER.
           COMPUTE WKS-AMOUNT ROUNDED = WKS-AMOUNT +
              SP-CAL-RATE(CTBL CSTP)
                / (WKS-INTMETHOD-DAYS * 100) * WKS-AMOUNT-1
              SIZE ERROR MOVE "E" TO COP-RETURN-CODE.
           GO TO CEI-DUE-LOOP.

       CEI-DUE-FINAL.
           COMPUTE WKS-ODD-1ST-PMT = WKS-REG-PAYMENT
                        + WKS-AMOUNT * COP-1ST-PMT-EXTENSION
             SIZE ERROR MOVE "E" TO COP-RETURN-CODE.

       CEI-EXIT.
           EXIT.

      ********************************
       CGA-CALC-LHINS  SECTION.
      ********************************
           MOVE 01 TO COPIX.
           IF COP-INS-IS-INPUT(COPIX) AND
              COP-INS-USE-IS-MEMO(COPIX)
              MOVE 0 TO COP-INS-PREMIUM(COPIX)
                        COP-INS-TAX(COPIX)
              GO TO CGA-EXIT.
           IF COP-INS-DL
              IF COP-INS-SINGLE
                 SET ILEV TO 1
                 PERFORM CMA-ALL-INS-CALCS
              ELSE
                 IF COP-INS-JOINT
                    SET ILEV TO 2
                    PERFORM CMA-ALL-INS-CALCS
                 ELSE
                    NEXT SENTENCE
           ELSE
           IF COP-INS-LL
              IF COP-INS-SINGLE
                 SET ILEV TO 3
                 PERFORM CMA-ALL-INS-CALCS
              ELSE
                 IF COP-INS-JOINT
                    SET ILEV TO 4
                    PERFORM CMA-ALL-INS-CALCS.

       CGA-EXIT.
           EXIT.

      ********************************
       CIA-CALC-AHINS  SECTION.
      ********************************
           MOVE 02 TO COPIX. 
           IF COP-INS-IS-INPUT(COPIX) AND
              COP-INS-USE-IS-MEMO(COPIX)
              MOVE 0 TO COP-INS-PREMIUM(COPIX)
                        COP-INS-TAX(COPIX)
              GO TO CIA-EXIT.
           IF COP-INS-AH
              SET ILEV TO 5
              PERFORM CMA-ALL-INS-CALCS.

       CIA-EXIT.
           EXIT.


      ********************************
       CKA-CALC-PPINS  SECTION.
      ********************************
           MOVE 03 TO COPIX.
           IF COP-INS-IS-INPUT(COPIX) AND
              COP-INS-USE-IS-MEMO(COPIX)
              MOVE 0 TO COP-INS-PREMIUM(COPIX)
                        COP-INS-TAX(COPIX)
              GO TO CKA-EXIT.
           IF COP-INS-PP
              SET ILEV TO 6
              PERFORM CMA-ALL-INS-CALCS.
           IF COP-INS-PP2
              SET ILEV TO 7
              PERFORM CMA-ALL-INS-CALCS.

       CKA-EXIT.
           EXIT.

      ***********************************
       CLA-CALC-O1-INS  SECTION.
      ***********************************
           MOVE 04 TO COPIX.
           IF COP-INS-IS-INPUT(COPIX) AND
              COP-INS-USE-IS-MEMO(COPIX)
              MOVE 0 TO COP-INS-PREMIUM(COPIX)
                        COP-INS-TAX(COPIX)
              GO TO CLA-EXIT.
           IF COP-INS-O1
              SET ILEV TO 8
              PERFORM CMA-ALL-INS-CALCS.

       CLA-EXIT.
           EXIT.

      *********************************************
      *    COMPUTE INSURANCE SALES TAX FOR O2
      *********************************************
       CLA-CALC-O2-INS-TAX  SECTION.
           MOVE 05 TO COPIX.
           IF COP-INS-PREMIUM(COPIX) = 0 OR
              COP-INS-USE-IS-MEMO(COPIX)
              MOVE 0 TO COP-INS-TAX(COPIX)
           ELSE
              SET ILEV TO 9
              PERFORM COW-INS-TAX.
   
      *********************************************
      *    COMPUTE INSURANCE SALES TAX FOR O3
      *  JTG 050224 ALLOW MEMO 'O3' INSURANCE AND CALCULATE KENTUCKY SALES TAX
      *********************************************
       CLA-CALC-O3-INS-TAX  SECTION.
           MOVE 06 TO COPIX. 
           IF COP-INS-PREMIUM(COPIX) = 0
      *       OR COP-INS-USE-IS-MEMO(COPIX)
              MOVE 0 TO COP-INS-TAX(COPIX)
           ELSE
              SET ILEV TO 10
              PERFORM COW-INS-TAX.
   

      **********************************************
      *    COMPUTE ALL INSURANCES
      **********************************************
       CMA-ALL-INS-CALCS  SECTION.
           IF COP-INS-NO-ONE
              GO TO CMA-EXIT.
           IF COP-INS-IS-CALCD(COPIX)
              MOVE "Y" TO COP-INS-USE-SW(COPIX).

           IF ( COP-INS-CO(COPIX) = 0
                 AND NOT COP-INS-USE-IS-MEMO(COPIX) )
             OR COP-INS-IS-CALCD(COPIX)
                MOVE SP-INSCOM(ILEV) TO COP-INS-CO(COPIX).

           IF COP-INS-IS-CALCD(COPIX)
            OR ( COP-INS-IS-INPUT(COPIX)
                               AND NOT COP-TERM-CHANGED(COPIX) )
              IF ( SP-INSREJECT-FG(ILEV) = "C"        ) AND
                 ( COP-INS-REJECT-TERM(COPIX) NOT = 0 )
                 MOVE COP-INS-REJECT-TERM(COPIX)
                                  TO COP-INS-TERM(COPIX)
                 IF COP-INS-TERM(COPIX) = 9999
                    GO TO CMA-ERROR-EXIT
                 END-IF
              ELSE
                 IF SP-INSURE-EXT-PERIOD(ILEV) AND
                    COP-1ST-PMT-MTHS > 0
                    COMPUTE COP-INS-TERM(COPIX) = COP-TERM
                                            + COP-1ST-PMT-MTHS - 1
                 ELSE
                    MOVE COP-TERM TO COP-INS-TERM(COPIX).
           IF COP-INS-TERM(COPIX) > SP-INSMAXTERM(ILEV)
              IF NOT SP-INS-ADJ-PRIN-HALFINT-TFC(ILEV)
                 MOVE SP-INSMAXTERM(ILEV) TO COP-INS-TERM(COPIX)
              ELSE
                 GO TO CMA-ERROR-EXIT.

           IF COP-INS-TERM(COPIX) < 1
              MOVE 1 TO COP-INS-TERM(COPIX).

           IF COP-INS-IS-INPUT(COPIX) AND COP-COVERAGE-CHANGED(COPIX)
              MOVE COP-INS-COVERAGE(COPIX) TO IWA-INS-CHARGEABLE
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-PRINCIPAL(ILEV)
                  OR SP-INS-ADJ-PRIN-NO1INT(ILEV)
              MOVE COP-AMT-FINANCD TO IWA-INS-CHARGEABLE
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-ADJ-PRINCIPAL(ILEV)
              SUBTRACT COP-INT-CHARGE FROM COP-TOTAL-NOTE
                                       GIVING IWA-INS-CHARGEABLE
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-ADJ-PRIN1INT(ILEV)
              COMPUTE IWA-INS-CHARGEABLE ROUNDED =
                  (COP-TOTAL-NOTE - COP-INT-CHARGE)
                              * (1 + COP-SIM-RATE / 1200)
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-ADJ-PRIN-HALFINT-TFC(ILEV)
              COMPUTE IWA-INS-CHARGEABLE ROUNDED =
                  COP-AMT-FINANCD + COP-SERVICE-CHG
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-TN-INT-CL(ILEV)
              COMPUTE IWA-INS-CHARGEABLE ROUNDED =
                  COP-PROCEEDS + COP-GL-AMOUNT-TOTAL + COP-SERVICE-CHG
                + WKS-PRIOR-LOAN-BALANCE
                + COP-INS-PREMIUM(2) + COP-INS-PREMIUM(3)
                + COP-INS-PREMIUM(4) + COP-INS-PREMIUM(5)
                + COP-INS-PREMIUM(6) + COP-INS-PREMIUM(7)
                + COP-INS-PREMIUM(8)
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-TN-INT-SC-CL(ILEV)
              COMPUTE IWA-INS-CHARGEABLE ROUNDED =
                  COP-PROCEEDS + COP-GL-AMOUNT-TOTAL
                + COP-INS-PREMIUM(2) + COP-INS-PREMIUM(3)
                + COP-INS-PREMIUM(4) + COP-INS-PREMIUM(5)
                + COP-INS-PREMIUM(6) + COP-INS-PREMIUM(7)
                + COP-INS-PREMIUM(8)
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-ADJ-PRIN-HALFINT(ILEV)
              COMPUTE IWA-INS-CHARGEABLE ROUNDED =
                   COP-AMT-FINANCD
                              * (1 + COP-SIM-RATE / 1200) ** 0.5
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-MAX-TERM-BENEFIT(ILEV)
              IF COP-TERM > SP-INSMAXTERM(ILEV)
                 IF COP-REG-PAYMENT > SP-INSMAXBENIF(ILEV)
      * BOTH TERM AND PAYMENT EXCEED MAX
                    COMPUTE IWA-INS-CHARGEABLE =
                          ( SP-INSMAXTERM(ILEV) * SP-INSMAXBENIF(ILEV) )
                                - COP-MAINT-FEES - COP-1ST-PY-EXT
                    GO TO CMA-PERFORM-INS-ROUTINES
                 ELSE
      * TERM EXCEEDS MAX BUT PAYMENT DOES NOT
                    COMPUTE IWA-INS-CHARGEABLE =
                          ( SP-INSMAXTERM(ILEV) * COP-REG-PAYMENT )
                                - COP-MAINT-FEES - COP-1ST-PY-EXT
                    GO TO CMA-PERFORM-INS-ROUTINES
              ELSE
                 IF COP-REG-PAYMENT > SP-INSMAXBENIF(ILEV)
      * TERM DOES NOT EXCEED MAX BUT PAYMENT DOES
                    COMPUTE IWA-INS-CHARGEABLE =
                          ( COP-TERM * SP-INSMAXBENIF(ILEV) )
                                - COP-MAINT-FEES - COP-1ST-PY-EXT
                    GO TO CMA-PERFORM-INS-ROUTINES
                  ELSE
                     MOVE COP-TOTAL-NOTE TO IWA-INS-CHARGEABLE
                     GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-TOTAL-NOTE(ILEV)
              MOVE COP-TOTAL-NOTE TO IWA-INS-CHARGEABLE
              IF NOT (CD-CCY-TYPE = " " AND CD-CCT-TYPE = " ")
                 SUBTRACT COP-INS-TAX(1) COP-INS-TAX(2)
                          COP-INS-TAX(3) COP-INS-TAX(4)
                          COP-INS-TAX(5) COP-INS-TAX(6)
                          COP-INS-TAX(7) COP-INS-TAX(8)
                 FROM IWA-INS-CHARGEABLE
              END-IF
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF ( SP-INS-BENEFIT-TERM(ILEV)     ) OR
              ( SP-INS-MIP(ILEV) AND ILEV = 5 )
              COMPUTE IWA-INS-CHARGEABLE =
                   COP-TOTAL-NOTE / COP-TERM
                               * COP-INS-TERM(COPIX)
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-TOTAL-NOTE-EXT(ILEV)
              ADD COP-TOTAL-NOTE COP-1ST-PY-EXT
                                   GIVING IWA-INS-CHARGEABLE
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-ALL(ILEV)
              MOVE COP-TOT-REPYBLE TO IWA-INS-CHARGEABLE
              GO TO CMA-PERFORM-INS-ROUTINES.

      * SIGNAL (WISC) PR# 2157
           IF SP-INS-AMTFIN-2PMTS(ILEV)
              COMPUTE IWA-INS-CHARGEABLE =
                  COP-AMT-FINANCD + (2 * COP-REG-PAYMENT)
              GO TO CMA-PERFORM-INS-ROUTINES.

      * SIGNAL (WISC) PR# 2157
           IF SP-INS-AMTFIN-1PMTS(ILEV)
              COMPUTE IWA-INS-CHARGEABLE =
                  COP-AMT-FINANCD + COP-REG-PAYMENT
              GO TO CMA-PERFORM-INS-ROUTINES.

           MOVE COP-NEW-LOAN-PROCEEDS TO IWA-INS-CHARGEABLE.

           IF SP-INS-CASH-ADVANCE(ILEV)
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-CASH-ADV-FEES(ILEV)
              SUBTRACT COP-GL-AMOUNT-TOTAL FROM IWA-INS-CHARGEABLE
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF ( SP-INS-SP-CASH-ADVANCE(ILEV)                 ) OR
      * BADDDDDD IF STATEMENT, ILEV = 2 CAUSED ENTIRE IF TO BE TRUE
      *       ( SP-INS-MIP(ILEV) AND ILEV = 1 OR 2 OR 3 OR 4 )
              ( SP-INS-MIP(ILEV) AND (ILEV = 1 OR 2 OR 3 OR 4) )
              ADD COP-SERVICE-CHG TO IWA-INS-CHARGEABLE
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-ADJ-PRIN-3PMTS(ILEV)
              COMPUTE IWA-INS-CHARGEABLE =
                  COP-AMT-FINANCD + (3 * COP-REG-PAYMENT)
              GO TO CMA-PERFORM-INS-ROUTINES.

           IF SP-INS-NET-PAYOFF(ILEV)
              COMPUTE IWA-INS-CHARGEABLE = COP-AMT-FINANCD
              GO TO CMA-PERFORM-INS-ROUTINES.

      * PACESETTER PR# 2771
           IF SP-INS-CHGABL-MAX-TERM(ILEV)
              COMPUTE IWA-INS-CHARGEABLE =
                  COP-INS-TERM(COPIX) * COP-REG-PAYMENT
              GO TO CMA-PERFORM-INS-ROUTINES.



           GO TO CMA-ERROR-EXIT.

       CMA-PERFORM-INS-ROUTINES.
           IF IWA-INS-CHARGEABLE < SP-INSMINCHGABL(ILEV)
              MOVE SP-INSMINCHGABL(ILEV) TO IWA-INS-CHARGEABLE.
           IF SP-INS-ADJ-PRIN-HALFINT-TFC(ILEV)
              IF IWA-INS-CHARGEABLE > SP-INSMAXCHGABL(ILEV)
                 GO TO CMA-ERROR-EXIT.
           IF NOT SP-INSMAXCHGABL-OVRIDE(ILEV)
              IF IWA-INS-CHARGEABLE > SP-INSMAXCHGABL(ILEV)
                 IF SP-CL-AH-MAXIMUMS(ILEV)
                    GO TO CMA-ERROR-EXIT
                 ELSE
                    MOVE SP-INSMAXCHGABL(ILEV) TO IWA-INS-CHARGEABLE.

           IF ILEV = 5 OR 8
              DIVIDE COP-INS-TERM(COPIX) INTO IWA-INS-CHARGEABLE
                                        GIVING IWA-INS-BENEFIT ROUNDED
              IF ILEV = 8 AND (SP-INSMAXBENIF(8) = 0 OR 999999 )
                 NEXT SENTENCE
              ELSE
                 IF IWA-INS-BENEFIT > SP-INSMAXBENIF(ILEV)
                     IF ILEV = 5 AND SP-INS-MIP(ILEV)
                        MOVE SP-INSMAXBENIF(ILEV) TO IWA-INS-BENEFIT
                     END-IF
                     MULTIPLY SP-INSMAXBENIF(ILEV) BY
                       COP-INS-TERM(COPIX) GIVING IWA-INS-CHARGEABLE.

      * TEST FOR ADJUSTMENT TO COVERAGE:
           IF NOT ( SP-ADJ-INSCOV-BY-O2(ILEV) OR
                    SP-ADJ-INSCOV-BY-O3(ILEV) OR
                    SP-CALC-PREM-LESS-EXTCHG(ILEV)
                  )
              MOVE IWA-INS-CHARGEABLE TO COP-INS-COVERAGE(COPIX)
              GO TO CMA-SET-INS-PREMIUM.

           IF COP-INS-IS-INPUT(COPIX) AND COP-COVERAGE-CHANGED(COPIX)
              IF SP-CALC-PREM-LESS-EXTCHG(ILEV)
                 MOVE IWA-INS-CHARGEABLE TO COP-INS-COVERAGE(COPIX)
              ELSE
                 MOVE CEP-INS-COVERAGE(COPIX) TO IWA-INS-CHARGEABLE
           ELSE
              IF SP-CALC-PREM-LESS-EXTCHG(ILEV)
                 MOVE IWA-INS-CHARGEABLE TO COP-INS-COVERAGE(COPIX)
                 SUBTRACT COP-1ST-PY-EXT FROM IWA-INS-CHARGEABLE.
           IF SP-ADJ-INSCOV-BY-O2(ILEV)
              SUBTRACT COP-INS-COVERAGE(5) FROM IWA-INS-CHARGEABLE.
           IF SP-ADJ-INSCOV-BY-O3(ILEV)
              SUBTRACT COP-INS-COVERAGE(6) FROM IWA-INS-CHARGEABLE.

           IF IWA-INS-CHARGEABLE < 0
              MOVE 0 TO IWA-INS-CHARGEABLE
              GO TO CMA-ERROR-EXIT-3.

           IF COP-INS-IS-INPUT(COPIX) AND COP-COVERAGE-CHANGED(COPIX)
              IF COP-INS-COVERAGE(COPIX) > IWA-INS-CHARGEABLE
                 MOVE 0 TO IWA-INS-CHARGEABLE
                 GO TO CMA-ERROR-EXIT-3
              ELSE
                 MOVE COP-INS-COVERAGE(COPIX) TO IWA-INS-CHARGEABLE.

           IF NOT SP-CALC-PREM-LESS-EXTCHG(ILEV)
              MOVE IWA-INS-CHARGEABLE TO COP-INS-COVERAGE(COPIX).

      *KKK ESTABLISH INSURANCE PREMIUM:
       CMA-SET-INS-PREMIUM.
           IF SP-INS-ADJ-PRIN-3PMTS(ILEV)
              IF SP-PER-1000-PER-MONTH(ILEV)
                 IF NOT (COP-INS-IS-INPUT(COPIX)
                            AND COP-COVERAGE-CHANGED(COPIX)

                        )
                    COMPUTE IWA-INS-CHARGEABLE =
                        IWA-INS-CHARGEABLE - (3 * COP-REG-PAYMENT).

           IF SP-INS-ADJ-PRIN-HALFINT-TFC(ILEV)
              MOVE 367 TO ELAPSED-YRTYPE
              MOVE COP-IN-DT TO NUM-DATE
              MOVE COP-PMT-DT TO SYS-DATE
              PERFORM TIMALL
              COMPUTE IWA-INS-CHARGEABLE ROUNDED =
                  (COP-AMT-FINANCD + COP-SERVICE-CHG)
                      * (1 + 0.5 * COP-SIM-RATE / 1200)
                       * (1 + COP-SIM-RATE * ELAPSED-REM / 36500).


           IF COP-INS-IS-INPUT(COPIX) AND COP-PREMIUM-CHANGED(COPIX)
              MOVE COP-INS-PREMIUM(COPIX) TO IWA-INS
              PERFORM COW-SET-COP-INS
              GO TO CMA-EXIT.

      * TEST FOR STANDARD CALCULATIONS (1-4):
      * INSRATECD
           IF SP-PER-100-PER-ANNUM(ILEV)
                      OR SP-ADVANCE-MTG-FIRE(ILEV)
                       OR SP-100-ANNUM-EXTRA-30(ILEV)
              PERFORM CNA-PER-100-PER-ANNUM
              GO TO CMA-ADJUST-FOR-PRINCIPAL-ONLY
           ELSE
           IF SP-PER-1000-PER-MONTH(ILEV)
              PERFORM CNJ-PER-1000-PER-MONTH
              GO TO CMA-ADJUST-FOR-PRINCIPAL-ONLY
           ELSE
           IF SP-TABLE-INTERPOLATE(ILEV)
              PERFORM COA-TABLE-INTERPOLATE
              GO TO CMA-ADJUST-FOR-PRINCIPAL-ONLY
           ELSE
           IF SP-TABLE-ROUND-UP(ILEV)
              PERFORM COJ-TABLE-ROUND-UP
              GO TO CMA-ADJUST-FOR-PRINCIPAL-ONLY.

           IF SP-TX-PP-FIRE(ILEV)
              PERFORM CTX-PER-1000-PER-CHARGEABLE
              GO TO CMA-ADJUST-FOR-PRINCIPAL-ONLY
           ELSE
           IF SP-VA-PC-ACTUARIAL78(ILEV)
              PERFORM CVA5-PER-1000-PC
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-VA-IB-ACTUARIAL(ILEV)
                     OR SP-AL-NETPAYOFF(ILEV)
              PERFORM CVA6-PER-1000-IB
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-CA-NET-POFF-MULL(ILEV)
              PERFORM CVCA-NET-POFF-MULL
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-CA-NETPAYOFF-5A(ILEV)
              PERFORM CVCA-NETPAYOFF-5A
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-INS-RATE-FILE(ILEV)
              PERFORM COR-GET-INS-RATE-FILE
              GO TO CMA-ADJUST-FOR-PRINCIPAL-ONLY
           ELSE
           IF SP-VA-NET-POFF-FAMILY(ILEV)
              PERFORM CVVA-NET-POFF-FAMILY
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-RG-PA-NET(ILEV)
              PERFORM RGPA-NET-CL
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-RG-OH-NET(ILEV)
              PERFORM RGOH-NET-CL
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-RG-NY-NET(ILEV)
              PERFORM RGNY-NET-CL
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-MAINE-NET(ILEV)
              PERFORM MAINE-NET-CL
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-FREEDOM-NET-BALLOON(ILEV)
              PERFORM FREEDOM-NET-CL-BALLOON
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-RG-TN-NET(ILEV) OR
              SP-RG-KY-NET(ILEV)
              PERFORM RGTNKY-NET-CL
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-CA-TRUNC-NET-MULL(ILEV)
              PERFORM CA-TRUNC-NET-MULL
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-AL-NETPAY-PLUS1(ILEV)
              PERFORM AL-NET-PLUS1-WRLD
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-IN-NET-PACE(ILEV)
              PERFORM IN-NET-PACE
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-AL-1000-PER-MONTH(ILEV)
              PERFORM AL-STD-LIFE-PER-1000
              GO TO CMA-CALL-THE-COW
           ELSE
           IF SP-INSRATECD-25(ILEV)
              PERFORM CALC-INSRATECD-25
              GO TO CMA-CALL-THE-COW
           ELSE
              GO TO CMA-ERROR-EXIT.

       CMA-ADJUST-FOR-PRINCIPAL-ONLY.
           IF NOT SP-INSCHGABL-DECLBAL(ILEV)
              GO TO CMA-CALL-THE-COW.

           IF (COP-NO-OF-TIMES-CALLED > 1
                       OR COP-LOOP-COUNT > 1
              )
                    AND (SP-CAL-PROCEEDS(CTBL)
                                  OR SP-CAL-PRO-INS(CTBL)
                        )
              PERFORM CMB-GET-EQUIV-SIMPLE-SC
           ELSE
              MOVE WKS-ANNUITY TO IWA-LOAN-ANNUITY
              IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
                 MOVE WKS-RATE TO IWA-RATE
              ELSE
                 PERFORM XBA-CALCULATE-SIMPLE-INTEREST
                 MOVE APR-APR TO IWA-RATE.

      ***********************************************************
      * COP-SIM-RATE IS SET FOR CALCULATION OF CHARGEABLE
      *    RE:
      *       SP-INS-ADJ-PRIN-HALFINT(ILEV)
      *       SP-INS-ADJ-PRIN-HALFINT-TFC(ILEV)
      *       SP-INS-ADJ-PRIN1INT(ILEV)
      ***********************************************************
           MOVE IWA-RATE TO COP-SIM-RATE.

           IF COP-TERM = COP-INS-TERM(COPIX)
              MOVE 0 TO IWA-INS-ANNUITY
           ELSE
              SUBTRACT COP-INS-TERM(COPIX) FROM COP-TERM
                                                  GIVING IWA-TERM
              COMPUTE IWA-INS-ANNUITY ROUNDED =
                  1200 * (1 - 1 / (1 + IWA-RATE / 1200) ** IWA-TERM)
                                                            / IWA-RATE
                  SIZE ERROR
                     MOVE 9999.9999 TO IWA-INS-ANNUITY
                     GO TO CMA-ERROR-EXIT-2.

           IF IWA-RATE = 0
              GO TO CMA-CALL-THE-COW.

           IF NOT SP-GERMANTOWN-CO(ILEV)
              MOVE IWA-LOAN-ANNUITY TO WKS-INS-ANNUITY.

           IF SP-INS-ADJ-PRIN-3PMTS(ILEV)
              COMPUTE WKS-98V99 ROUNDED =
                  WKS-INSRATE-1000 * .001
                         * (
                            (COP-INS-TERM(COPIX)
                               - WKS-INS-ANNUITY
                                   + IWA-INS-ANNUITY)
                                               / (IWA-RATE / 1200)
                                          + (3 * COP-TERM)
                           ) * COP-REG-PAYMENT
           ELSE
           IF SP-INS-NET-PAYOFF(ILEV)
              COMPUTE WKS-98V99 ROUNDED =
                  WKS-INSRATE-1000 * .001
                         * (
                            (COP-INS-TERM(COPIX)
                               - WKS-INS-ANNUITY
                                   + IWA-INS-ANNUITY)
                                               / (IWA-RATE / 1200)
                           ) * COP-REG-PAYMENT
           ELSE
              COMPUTE WKS-98V99 ROUNDED = WKS-98V99
                    * (COP-INS-TERM(COPIX) - WKS-INS-ANNUITY
                                              + IWA-INS-ANNUITY)
                             / (IWA-RATE / 1200 * WKS-INS-ANNUITY)
               SIZE ERROR GO TO CMA-ERROR-EXIT.

           IF SP-INSDISCNT(ILEV) NOT = 0
              GO TO CMA-MOB-DISCOUNTED.

           COMPUTE IWA-INS ROUNDED = WKS-98V99
               SIZE ERROR GO TO CMA-ERROR-EXIT-2.
           GO TO CMA-CALL-THE-COW.

       CMA-MOB-DISCOUNTED.
           IF SP-INS-ADJ-PRIN1INT(ILEV)
                     OR SP-GERMANTOWN-CO(ILEV)
                      OR SP-INS-ADJ-PRIN-NO1INT(ILEV)
                       OR SP-INS-ADJ-PRIN-HALFINT(ILEV)
                        OR SP-INS-ADJ-PRIN-HALFINT-TFC(ILEV)
                         OR (SP-CAL-PROCEEDS(CTBL)
                          OR SP-CAL-PRO-INS(CTBL))
              COMPUTE IWA-INS ROUNDED =
                  WKS-98V99 * 2 / (COP-INS-TERM(COPIX) + 1)
                  SIZE ERROR GO TO CMA-ERROR-EXIT-2
           ELSE
              COMPUTE IWA-INS ROUNDED =
                  WKS-98V99 * 2 * (1 + IWA-RATE / 1200)
                              / (COP-INS-TERM(COPIX) + 1)
                  SIZE ERROR GO TO CMA-ERROR-EXIT-2.

       CMA-CALL-THE-COW.
           PERFORM COW-SET-COP-INS.
           GO TO CMA-EXIT.


       CMA-ERROR-EXIT.
           PERFORM COX-INS-ERROR.
           GO TO CMA-EXIT.

       CMA-ERROR-EXIT-2.
           PERFORM COY-INS-ERROR.
           GO TO CMA-EXIT.

       CMA-ERROR-EXIT-3.
           MOVE "R" TO COP-RETURN-CODE.
           MOVE 0   TO COP-INS-PREMIUM(COPIX).

       CMA-EXIT.
           EXIT.

      *********************************************
       CMB-GET-EQUIV-SIMPLE-SC  SECTION.
      *********************************************
           IF SP-INS-ADJ-PRIN-HALFINT-TFC(ILEV)
              MOVE COP-INT-CHARGEABLE TO APR-FINANCED
           ELSE
              ADD COP-INT-CHARGEABLE COP-SERVICE-CHG
                                        GIVING APR-FINANCED.
           COMPUTE APR-REGPYAMT ROUNDED =
               (APR-FINANCED + COP-INT-CHARGE) / COP-TERM.
           MOVE COP-TERM TO APR-NOREGPY.
           MOVE 0 TO APR-NOODDPY APR-1STPYAMT.
           MOVE COP-IN-DT TO APR-INTDATE.
           MOVE COP-PMT-DT TO APR-1STPYDATE.
           PERFORM ZCA-APR-REGZ.
           MOVE APR-APR TO IWA-RATE.
           COMPUTE COP-STATE-WORKER ROUNDED = APR-APR.
           COMPUTE IWA-LOAN-ANNUITY =
              (1 - (1 / (1 + IWA-RATE / 1200) ** COP-TERM)
              ) / (IWA-RATE / 1200).
           MOVE IWA-LOAN-ANNUITY TO WKS-BAH-ANNUITY.
           MOVE IWA-RATE TO WKS-BAH-RATE.
           IF NOT SP-INS-ADJ-PRIN-HALFINT-TFC(ILEV)
              PERFORM BAH-ALL-PAYMENT-EXTENSION
              MOVE WKS-BAH-ANNUITY TO IWA-LOAN-ANNUITY.

       CMB-EXIT.
           EXIT.

      *********************************************
       CNA-PER-100-PER-ANNUM  SECTION.
      *********************************************
      * REGMGM PR# 3553 3/1/2010
      *   RATE/$100/ANNUM, CONSIDERING EXTRA DAYS MORE THAN 30 DAYS FROM THE
      *   FIRST PAYMENT. ASSURANT GAVE THE FORMULA.
           IF SP-100-ANNUM-EXTRA-30(ILEV)
              MOVE 360 TO ELAPSED-YRTYPE
              MOVE CIP-IN-DT TO NUM-DATE
              MOVE CIP-PMT-DT TO SYS-DATE NDTE-DATE
              PERFORM TIMALL
              COMPUTE WKS-98V99 ROUNDED =
                ( (SP-INSRATE(ILEV,1)/100) *
               ((COP-INS-TERM(COPIX) + (( ELAPSED-DAYS - 30 ) / 30 ))
                         / 12) )
                SIZE ERROR GO TO CNA-ERROR-EXIT
              END-COMPUTE
           ELSE
              COMPUTE WKS-98V99 ROUNDED =
                SP-INSRATE(ILEV,1) * COP-INS-TERM(COPIX) / 1200
                SIZE ERROR GO TO CNA-ERROR-EXIT.

           IF NOT (SP-INSJFAC(ILEV) = 0
                   OR (SP-INSJFAC(ILEV) = 100))
             COMPUTE WKS-98V99 ROUNDED = WKS-98V99
               * SP-INSJFAC(ILEV) * .01
               SIZE ERROR GO TO CNA-ERROR-EXIT.

           IF SP-ADVANCE-MTG-FIRE(ILEV)
              COMPUTE IWA-INS ROUNDED =
                 (IWA-INS-CHARGEABLE - COP-INS-PREMIUM(COPIX))
                                                   * WKS-98V99
              COMPUTE IWA-INS ROUNDED =
                 (IWA-INS-CHARGEABLE
                             - COP-INS-PREMIUM(COPIX) + IWA-INS)
                                                * WKS-98V99
           ELSE
              IF NOT ( (SP-ORGST = "LA") AND (COPIX = 1 OR 3) )
                 COMPUTE IWA-INS = IWA-INS-CHARGEABLE * WKS-98V99
                         SIZE ERROR GO TO CNA-ERROR-EXIT
                 END-COMPUTE
                 GO TO CNA-EXIT
              END-IF
              COMPUTE IWA-INS ROUNDED = IWA-INS-CHARGEABLE * WKS-98V99
                      SIZE ERROR GO TO CNA-ERROR-EXIT.
           GO TO CNA-EXIT.


       CNA-ERROR-EXIT.
           PERFORM COY-INS-ERROR.

       CNA-EXIT.
           EXIT.

      *********************************************
       CTX-PER-1000-PER-CHARGEABLE  SECTION.
      *********************************************
           MOVE IWA-INS-CHARGEABLE TO HOLD-INS-CHARGEABLE.

      * 1.80 * CHARGEABLE/100 * YRS FOR 1ST 1000.00

           IF IWA-INS-CHARGEABLE > 1000
              MOVE 1000.00 TO HOLD-INS-CHARGEABLE.

           MOVE 1.80 TO CTX-INSRATE.
           PERFORM CTX-CALC-PREMIUM.

      *----------------------------------------------------------------
      * 1.35 * CHARGEABLE/100 * YRS FOR 2ND 1000.00
      *----------------------------------------------------------------
           IF  NOT ( IWA-INS-CHARGEABLE > 1000 )
               GO TO CTX-EXIT
           ELSE
              IF  IWA-INS-CHARGEABLE > 2000
                  MOVE 1000.00 TO HOLD-INS-CHARGEABLE
              ELSE
                  COMPUTE HOLD-INS-CHARGEABLE =
                      IWA-INS-CHARGEABLE - 1000.

           MOVE IWA-INS TO HOLD-IWA-INS.
           MOVE 1.35 TO CTX-INSRATE.
           PERFORM CTX-CALC-PREMIUM.
           ADD IWA-INS TO HOLD-IWA-INS.

      *----------------------------------------------------------------
      * 0.90 * CHARGEABLE/100 * YRS FOR AMOUNTS OVER 2000.00
      *----------------------------------------------------------------
           IF NOT ( IWA-INS-CHARGEABLE > 2000)
              MOVE HOLD-IWA-INS TO IWA-INS
              GO TO CTX-EXIT
           ELSE
              COMPUTE HOLD-INS-CHARGEABLE =
                IWA-INS-CHARGEABLE - 2000.

           MOVE 0.90 TO CTX-INSRATE.
           PERFORM CTX-CALC-PREMIUM.
           ADD HOLD-IWA-INS TO IWA-INS.
           GO TO CTX-EXIT.

       CTX-CALC-PREMIUM.
           COMPUTE WKS-98V99 ROUNDED =
             CTX-INSRATE * COP-INS-TERM(COPIX) / 1200
             SIZE ERROR GO TO CTX-ERROR-EXIT.
           IF NOT (SP-INSJFAC(ILEV) = 0
                   OR (SP-INSJFAC(ILEV) = 100))
             COMPUTE WKS-98V99 ROUNDED = WKS-98V99
               * SP-INSJFAC(ILEV) * .01
               SIZE ERROR GO TO CTX-ERROR-EXIT.
           COMPUTE IWA-INS ROUNDED = HOLD-INS-CHARGEABLE
             * WKS-98V99
             SIZE ERROR GO TO CTX-ERROR-EXIT.

       CTX-ERROR-EXIT.
           PERFORM COY-INS-ERROR.

       CTX-EXIT.
           EXIT.


      ************************************************************
      *    RATE PER 1000 PER MONTH
      *
      ************************************************************
       CNJ-PER-1000-PER-MONTH  SECTION.
           MOVE SP-INSRATE(ILEV, 1) TO WKS-INSRATE-1000.

           IF SP-INS-PRIN-CALC-MOB(ILEV)
              COMPUTE WKS-INSRATE-1000 ROUNDED =
                  WKS-INSRATE-1000 * COP-INS-TERM(COPIX) / 12
                        * 20 / (COP-INS-TERM(COPIX) + 1).

           IF NOT SP-INS-ADJ-PRIN-3PMTS(ILEV)
              GO TO CNJ-TEST-INSDISCNT.

           SET ISTP TO 1.
           SEARCH SP-INSRATETBL
             END PERFORM COX-INS-ERROR
                 GO TO CNJ-TEST-INSDISCNT
             WHEN SP-INSTERM(ILEV,ISTP) NOT < COP-INS-TERM(COPIX)
                  NEXT SENTENCE.
             MOVE SP-INSRATE(ILEV,ISTP) TO WKS-INSRATE-1000.

       CNJ-TEST-INSDISCNT.
           IF SP-INSDISCNT(ILEV) NOT = 0
              GO TO CNJ-CONTINUE.

           COMPUTE WKS-98V99 ROUNDED
                   IWA-INS   ROUNDED =
               IWA-INS-CHARGEABLE * WKS-INSRATE-1000 * .001
               SIZE ERROR GO TO CNJ-ERROR-EXIT.
      *    MOVE WKS-98V99 TO IWA-INS.
           GO TO CNJ-EXIT.

       CNJ-CONTINUE.
           IF SP-GERMANTOWN-CO(ILEV)
              GO TO CNJ-GERMANTOWN.
           COMPUTE WKS-98V99 ROUNDED =
               1 + SP-INSDISCNT(ILEV)
                           * COP-INS-TERM(COPIX) / 2400
               SIZE ERROR GO TO CNJ-ERROR-EXIT.
           COMPUTE WKS-98V99 ROUNDED =
               WKS-INSRATE-1000 * (COP-INS-TERM(COPIX) + 1)
                                           / (20 * WKS-98V99)
               SIZE ERROR GO TO CNJ-ERROR-EXIT.
           IF SP-SECURITY-INS-CO(ILEV)
              ADD 0 WKS-98V99 GIVING WKS-98V92 ROUNDED
              MOVE WKS-98V92 TO WKS-98V99.
           IF NOT ( SP-INSJFAC(ILEV) = 0
                 OR (SP-INSJFAC(ILEV) = 100) )
              COMPUTE WKS-98V99 ROUNDED =
                  WKS-98V99 * SP-INSJFAC(ILEV) * .01
                  SIZE ERROR GO TO CNJ-ERROR-EXIT.
           IF SP-SECURITY-INS-CO(ILEV)
              ADD 0 WKS-98V99 GIVING WKS-98V92 ROUNDED
              MOVE WKS-98V92 TO WKS-98V99.
           COMPUTE IWA-INS ROUNDED =
               IWA-INS-CHARGEABLE * WKS-98V99 * .01
               SIZE ERROR GO TO CNJ-ERROR-EXIT.
           GO TO CNJ-END.

       CNJ-GERMANTOWN.
           MOVE SP-INSDISCNT(ILEV) TO WKS-98V92.
           IF COP-INS-TERM(COPIX) > 121
              ADD .1 TO WKS-98V92.
           COMPUTE WKS-98V99 ROUNDED =
               1 + WKS-98V92
                           * COP-INS-TERM(COPIX) / 2400
               SIZE ERROR GO TO CNJ-ERROR-EXIT.
           COMPUTE WKS-98V93 =
               WKS-INSRATE-1000 / WKS-98V99 + .0005
               SIZE ERROR GO TO CNJ-ERROR-EXIT.
           COMPUTE IWA-INS ROUNDED =
               WKS-98V93 * IWA-INS-CHARGEABLE * .0005
                                     * (COP-INS-TERM(COPIX) + 1).
       CNJ-END.
           MOVE IWA-INS TO WKS-98V99.
           GO TO CNJ-EXIT.

       CNJ-ERROR-EXIT.
           PERFORM COY-INS-ERROR.

       CNJ-EXIT.
           EXIT.


      *********************************************
       COA-TABLE-INTERPOLATE  SECTION.
      *********************************************
           SET ISTP TO 1.
           SEARCH SP-INSRATETBL
             END PERFORM COX-INS-ERROR
                 GO TO COA-EXIT
             WHEN SP-INSTERM(ILEV,ISTP) = COP-INS-TERM(COPIX)
                  GO TO COA-EASY-CALC
             WHEN SP-INSTERM(ILEV,ISTP) > COP-INS-TERM(COPIX)
                  GO TO COA-INTERPOLATE.

       COA-EASY-CALC.
           COMPUTE WKS-98V99 ROUNDED =
             SP-INSRATE(ILEV,ISTP) * .01
             SIZE ERROR GO TO COA-ERROR-EXIT.
           IF NOT (SP-INSJFAC(ILEV) = 0
                   OR (SP-INSJFAC(ILEV) = 100))
             COMPUTE WKS-98V99 ROUNDED = WKS-98V99
               * SP-INSJFAC(ILEV) * .01
               SIZE ERROR GO TO COA-ERROR-EXIT.
           COMPUTE IWA-INS ROUNDED = IWA-INS-CHARGEABLE
             * WKS-98V99
             SIZE ERROR GO TO COA-ERROR-EXIT.
           GO TO COA-EXIT.

       COA-INTERPOLATE.
           IF ISTP > 1
             GO TO COA-BIG-FORMULA.
           COMPUTE WKS-98V99 ROUNDED =
             SP-INSRATE(ILEV,ISTP) / SP-INSTERM(ILEV,ISTP)
             * COP-INS-TERM(COPIX) * .01
             SIZE ERROR GO TO COA-ERROR-EXIT.
           IF NOT (SP-INSJFAC(ILEV) = 0
                   OR (SP-INSJFAC(ILEV) = 100))
             COMPUTE WKS-98V99 ROUNDED = WKS-98V99
               * SP-INSJFAC(ILEV) * .01
               SIZE ERROR GO TO COA-ERROR-EXIT.
           COMPUTE IWA-INS ROUNDED = IWA-INS-CHARGEABLE
             * WKS-98V99
             SIZE ERROR GO TO COA-ERROR-EXIT.
           GO TO COA-EXIT.

       COA-BIG-FORMULA.
           COMPUTE WKS-98V99 ROUNDED =
             (SP-INSRATE(ILEV,ISTP - 1)
             + (SP-INSRATE(ILEV,ISTP) - SP-INSRATE(ILEV,ISTP - 1))
             / (SP-INSTERM(ILEV,ISTP) - SP-INSTERM(ILEV,ISTP - 1))
             * (COP-INS-TERM(COPIX)
             - SP-INSTERM(ILEV,ISTP - 1))) * .01
             SIZE ERROR GO TO COA-ERROR-EXIT.
           IF NOT (SP-INSJFAC(ILEV) = 0
                   OR (SP-INSJFAC(ILEV) = 100))
             COMPUTE WKS-98V99 ROUNDED = WKS-98V99
               * SP-INSJFAC(ILEV) * .01
               SIZE ERROR GO TO COA-ERROR-EXIT.
           COMPUTE IWA-INS ROUNDED = IWA-INS-CHARGEABLE
             * WKS-98V99
             SIZE ERROR GO TO COA-ERROR-EXIT.
           GO TO COA-EXIT.

       COA-ERROR-EXIT.
           PERFORM COY-INS-ERROR.

       COA-EXIT.
           EXIT.


      *********************************************
       COJ-TABLE-ROUND-UP  SECTION.
      *********************************************
           SET ISTP TO 1.
           SEARCH SP-INSRATETBL
             END PERFORM COX-INS-ERROR
                 GO TO COJ-EXIT
             WHEN SP-INSTERM(ILEV,ISTP) NOT < COP-INS-TERM(COPIX)
                  NEXT SENTENCE.

           COMPUTE WKS-98V99 ROUNDED = SP-INSRATE(ILEV,ISTP) * .01
             SIZE ERROR GO TO COJ-ERROR-EXIT.

           IF NOT (SP-INSJFAC(ILEV) = 0
                   OR (SP-INSJFAC(ILEV) = 100))
             COMPUTE WKS-98V99 ROUNDED = WKS-98V99
               * SP-INSJFAC(ILEV) * .01
               SIZE ERROR GO TO COJ-ERROR-EXIT.

           COMPUTE IWA-INS = IWA-INS-CHARGEABLE * WKS-98V99
               SIZE ERROR GO TO COJ-ERROR-EXIT.

           GO TO COJ-EXIT.

       COJ-ERROR-EXIT.
           PERFORM COY-INS-ERROR.

       COJ-EXIT.
           EXIT.

      ***************************************************
      *    GET INSURANCE RATE TABLES
      ***************************************************
       COR-GET-INS-RATE-FILE  SECTION.
           IF COP-INS-TERM(COPIX) > 120
              GO TO COR-ERROR-EXIT.

           IF SP-INSRATECD(ILEV) NOT = IR-KEY
              MOVE SP-INSRATECD(ILEV) TO IR-KEY
              MOVE "CL/IRREAD" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH IR-REC
              CANCEL FORM-PROGX
              IF IR-KEY = 0
                 GO TO COR-ERROR-EXIT.

           IF IR-PER-FG = "T"
              MOVE 1000 TO WKS-WORKER
           ELSE
              MOVE 100  TO WKS-WORKER.


           SET IRIX TO COP-INS-TERM(COPIX).
           COMPUTE WKS-98V99 ROUNDED =
               IR-INSRATE(IRIX) / WKS-WORKER
               SIZE ERROR GO TO COR-ERROR-EXIT.
           IF NOT (SP-INSJFAC(ILEV) = 0
                          OR (SP-INSJFAC(ILEV) = 100)
                  )
              COMPUTE WKS-98V99 ROUNDED =
                  WKS-98V99 * SP-INSJFAC(ILEV) * .01
                  SIZE ERROR GO TO COR-ERROR-EXIT.
           IF ILEV = 5 AND SP-INS-MIP(ILEV)
              COMPUTE IWA-INS ROUNDED =
                  IWA-INS-BENEFIT * WKS-98V99
                  SIZE ERROR GO TO COR-ERROR-EXIT
           ELSE
              COMPUTE IWA-INS ROUNDED =
                  IWA-INS-CHARGEABLE * WKS-98V99
                  SIZE ERROR GO TO COR-ERROR-EXIT.
           GO TO COR-EXIT.

       COR-ERROR-EXIT.
           PERFORM COY-INS-ERROR.

       COR-EXIT.
           EXIT.

      *************************************************************
      *    FAMILY VIRGINIA CL NET PAYOFF  (VOYAGER)
      *************************************************************
       CVVA-NET-POFF-FAMILY  SECTION.
           MOVE COP-TERM TO CVVA-N.
           MOVE COP-INS-TERM(COPIX) TO CVVA-M.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              COMPUTE CVVA-I ROUNDED = WKS-RATE / 1200
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              COMPUTE CVVA-I ROUNDED = APR-APR / 1200.
           COMPUTE CVVA-A--N-I ROUNDED =
               (1 - 1 / (1 + CVVA-I) ** CVVA-N) / CVVA-I
                  SIZE ERROR GO TO CVVA-NET-POFF-FAMILY-ERROR.
           COMPUTE CVVA-A--N-M-I ROUNDED =
               (1 - 1 / (1 + CVVA-I) ** (CVVA-N - CVVA-M)) / CVVA-I
                  SIZE ERROR GO TO CVVA-NET-POFF-FAMILY-ERROR.

           COMPUTE CVVA-WK ROUNDED =
              (CVVA-M + 1) * SP-INSRATE(ILEV 1) * 12
               / (
                  20 * CVVA-M * (1 + (CVVA-M * SP-INSDISCNT(ILEV)
                                        / 2400
                                     )
                                )
                 )
                  SIZE ERROR GO TO CVVA-NET-POFF-FAMILY-ERROR.

           COMPUTE CVVA-WK ROUNDED =
              CVVA-WK * CVVA-M * (CVVA-M - CVVA-A--N-I + CVVA-A--N-M-I)
                    / (600 * (CVVA-M + 1) * CVVA-I * CVVA-A--N-I)
                  SIZE ERROR GO TO CVVA-NET-POFF-FAMILY-ERROR.

       CVVA-NET-POFF-FAMILY-END.
           COMPUTE IWA-INS ROUNDED =
                 CVVA-WK * IWA-INS-CHARGEABLE / (1 - CVVA-WK)
                  SIZE ERROR GO TO CVVA-NET-POFF-FAMILY-ERROR.
           GO TO CVVA-NET-POFF-FAMILY-EXIT.

       CVVA-NET-POFF-FAMILY-ERROR.
           PERFORM COY-INS-ERROR.

       CVVA-NET-POFF-FAMILY-EXIT.
           EXIT.

      *************************************************************
      *    REGIONAL MANAGMENT AL STF FAMILY PER 1000 PER MONTH
      *************************************************************
       AL-STD-LIFE-PER-1000  SECTION.
           MOVE COP-TERM TO CVVA-N.
           MOVE COP-INS-TERM(COPIX) TO CVVA-M.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              COMPUTE CVVA-I ROUNDED = WKS-RATE / 1200
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              COMPUTE CVVA-I ROUNDED = APR-APR / 1200.

      *--------------------------------------------------------------------
      * A>N =     1 - ( 1 / (1+I))^N          1 + I
      *           ------------------   X ----------------
      *                    I                (I * DFP)
      *                                  1 + --------
      *                                      DPY / 12
      *--------------------------------------------------------------------
           COMPUTE CVVA-A--N-I ROUNDED =
               (1 - 1 / (1 + CVVA-I) ** CVVA-N) / CVVA-I
                  SIZE ERROR GO TO AL-STD-LIFE-PER-1000-ERROR.

           COMPUTE CVVA-A--N-M-I ROUNDED =
               (1 - 1 / (1 + CVVA-I) ** (CVVA-N - CVVA-M)) / CVVA-I
                  SIZE ERROR GO TO AL-STD-LIFE-PER-1000-ERROR.

           COMPUTE CVVA-WK ROUNDED =
              (CVVA-M + 1) * SP-INSRATE(ILEV 1) * 12
               / (
                  20 * CVVA-M * (1 + (CVVA-M * SP-INSDISCNT(ILEV)
                                        / 2400
                                     )
                                )
                 )
                  SIZE ERROR GO TO AL-STD-LIFE-PER-1000-ERROR.

           COMPUTE CVVA-WK ROUNDED =
              CVVA-WK * CVVA-M * (CVVA-M - CVVA-A--N-I + CVVA-A--N-M-I)
                    / (600 * (CVVA-M + 1) * CVVA-I * CVVA-A--N-I)
                  SIZE ERROR GO TO AL-STD-LIFE-PER-1000-ERROR.

       AL-STD-LIFE-PER-1000-END.
           COMPUTE IWA-INS ROUNDED =
                 CVVA-WK * (WKS-REG-PAYMENT - WKS-MTHLY-MAINT-FEES)
                                * CVVA-A--N-I
                  SIZE ERROR GO TO AL-STD-LIFE-PER-1000-ERROR.
           GO TO AL-STD-LIFE-PER-1000-EXIT.

       AL-STD-LIFE-PER-1000-ERROR.
           PERFORM COY-INS-ERROR.

       AL-STD-LIFE-PER-1000-EXIT.
           EXIT.

      *************************************************************
      *    REGIONAL MANAGMENT AL STF FAMILY PER 1000 PER MONTH
      *************************************************************
       CALC-INSRATECD-25  SECTION.
           MOVE COP-IN-DT TO NUM-DATE.
           MOVE COP-PMT-DT TO SYS-DATE.
           MOVE SP-YEARTYPE TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
      *    MOVE ELAPSED-DAYS TO RG-K-DAYS.

           MOVE COP-TERM TO CVVA-N.
           MOVE COP-INS-TERM(COPIX) TO CVVA-M.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              COMPUTE CVVA-I ROUNDED = WKS-RATE / 1200
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              COMPUTE CVVA-I ROUNDED = APR-APR / 1200.

      *--------------------------------------------------------------------
      * A>N =     1 - ( 1 / (1+I))^N          1 + I
      *           ------------------   X ----------------
      *                    I                (I * DFP)
      *                                  1 + --------
      *                                      DPY / 12
      *--------------------------------------------------------------------
           COMPUTE CVVA-A--N-I ROUNDED =
               ((1 - 1 / (1 + CVVA-I) ** CVVA-N) / CVVA-I)
                 * ((1+CVVA-I)/(1+((CVVA-I * ELAPSED-DAYS)
                  /(SP-YEARTYPE / 12))))
                  SIZE ERROR GO TO CALC-INSRATECD-25-ERROR.

           COMPUTE CVVA-A--N-M-I ROUNDED =
               (1 - 1 / (1 + CVVA-I) ** (CVVA-N - CVVA-M)) / CVVA-I
                  SIZE ERROR GO TO CALC-INSRATECD-25-ERROR.

      * LPR
           COMPUTE CVVA-WK ROUNDED =
              (CVVA-M + 1) * SP-INSRATE(ILEV 1) * 12
               / (
                  20 * CVVA-M * (1 + (CVVA-M * SP-INSDISCNT(ILEV)
                                        / 2400
                                     )
                                )
                 )
                  SIZE ERROR GO TO CALC-INSRATECD-25-ERROR.

      * LP
           COMPUTE CVVA-WK ROUNDED =
              CVVA-WK * CVVA-M * (CVVA-M - CVVA-A--N-I + CVVA-A--N-M-I)
                    / (600 * (CVVA-M + 1) * CVVA-I * CVVA-A--N-I)
                  SIZE ERROR GO TO CALC-INSRATECD-25-ERROR.

           COMPUTE IWA-INS ROUNDED =
                 CVVA-WK * (WKS-REG-PAYMENT - WKS-MTHLY-MAINT-FEES)
                                * CVVA-A--N-I
                  SIZE ERROR GO TO CALC-INSRATECD-25-ERROR.
           GO TO CALC-INSRATECD-25-EXIT.

       CALC-INSRATECD-25-ERROR.
           PERFORM COY-INS-ERROR.

       CALC-INSRATECD-25-EXIT.
           EXIT.

      *************************************************************
      *    REGENCY'S PA NET PAYOFF AMORTIZED & TRUNCATED LOANS
      *              INSURANCE FORMULA
      *************************************************************
       RGPA-NET-CL  SECTION.

           MOVE COP-TERM TO RG-N.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              MOVE WKS-RATE TO IWA-RATE
              MOVE WKS-ANNUITY TO IWA-LOAN-ANNUITY
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              IF APR-APR = 0
                 MOVE RG-N TO IWA-LOAN-ANNUITY
              ELSE
                 MOVE APR-APR TO IWA-RATE
                 COMPUTE IWA-LOAN-ANNUITY ROUNDED =
                     (1 - 1 / (1 + IWA-RATE / 1200) ** RG-N)
                                              / (IWA-RATE / 1200)
                     SIZE ERROR GO TO RGPA-NET-CL-ERROR.

           COMPUTE RG-I ROUNDED = IWA-RATE / 1200.

           MOVE IWA-LOAN-ANNUITY TO RG-A--N.

           COMPUTE RG-SPR =
                SP-INSRATE(ILEV 1) * 0.01
                   / (1 + (SP-INSDISCNT(ILEV) * RG-N) / 2400)
                   * (RG-N - RG-A--N) / (RG-I * RG-A--N)
              SIZE ERROR GO TO RGPA-NET-CL-ERROR.

           COMPUTE RG-WK ROUNDED =
              RG-SPR * (1 + RG-I / 2)
                  / ( 1 - RG-SPR * (1 + RG-I / 2) ).

       RGPA-NET-CL-END.
           COMPUTE IWA-INS ROUNDED = IWA-INS-CHARGEABLE * RG-WK.
           GO TO RGPA-NET-CL-EXIT.

       RGPA-NET-CL-ERROR.
           PERFORM COY-INS-ERROR.

       RGPA-NET-CL-EXIT.
           EXIT.

      *************************************************************
      *    REGENCY'S OH OHIO NET PAYOFF AMORTIZED & TRUNCATED LOANS
      *              INSURANCE FORMULA
      *************************************************************
       RGOH-NET-CL  SECTION.

           MOVE COP-TERM TO RG-N.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              MOVE WKS-RATE TO IWA-RATE
              MOVE WKS-ANNUITY TO IWA-LOAN-ANNUITY
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              IF APR-APR = 0
                 MOVE RG-N TO IWA-LOAN-ANNUITY
              ELSE
                 MOVE APR-APR TO IWA-RATE
                 COMPUTE IWA-LOAN-ANNUITY ROUNDED =
                     (1 - 1 / (1 + IWA-RATE / 1200) ** RG-N)
                                              / (IWA-RATE / 1200)
                     SIZE ERROR GO TO RGOH-NET-CL-ERROR.

           COMPUTE RG-I ROUNDED = IWA-RATE / 1200.

           MOVE IWA-LOAN-ANNUITY TO RG-A--N.

           COMPUTE RG-SPR =
                SP-INSRATE(ILEV 1) * 0.01
                   * (RG-N - RG-A--N) / (RG-I * RG-A--N)
              SIZE ERROR GO TO RGOH-NET-CL-ERROR.

           COMPUTE RG-WK ROUNDED =
                IWA-INS-CHARGEABLE  / ( 1 - RG-SPR * (1 + RG-I) ).

       RGOH-NET-CL-END.
           COMPUTE IWA-INS ROUNDED = RG-SPR * RG-WK * (1 + RG-I).
           GO TO RGOH-NET-CL-EXIT.

       RGOH-NET-CL-ERROR.
           PERFORM COY-INS-ERROR.

       RGOH-NET-CL-EXIT.
           EXIT.

      ******************************************************************
      *    REGENCY'S NY NEW YORK NET PAYOFF AMORTIZED & TRUNCATED LOANS
      *              INSURANCE FORMULA
      ******************************************************************
       RGNY-NET-CL  SECTION.

           MOVE COP-TERM TO RG-N.
           SUBTRACT 1 FROM COP-TERM GIVING RG-N-1.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              MOVE WKS-RATE TO IWA-RATE
              COMPUTE IWA-LOAN-ANNUITY ROUNDED =
                  (1 - 1 / (1 + IWA-RATE / 1200) ** RG-N-1)
                                           / (IWA-RATE / 1200)
                     SIZE ERROR GO TO RGNY-NET-CL-ERROR
              END-COMPUTE
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              IF APR-APR = 0
                 MOVE RG-N-1 TO IWA-LOAN-ANNUITY
              ELSE
                 MOVE APR-APR TO IWA-RATE
                 COMPUTE IWA-LOAN-ANNUITY ROUNDED =
                     (1 - 1 / (1 + IWA-RATE / 1200) ** RG-N-1)
                                              / (IWA-RATE / 1200)
                     SIZE ERROR GO TO RGNY-NET-CL-ERROR.

           COMPUTE RG-I ROUNDED = IWA-RATE / 1200.

           MOVE IWA-LOAN-ANNUITY TO RG-A--N-1.

           COMPUTE RG-WK ROUNDED = (1 + RG-I) / (1 + RG-A--N-1)
              SIZE ERROR GO TO RGNY-NET-CL-ERROR.
           COMPUTE RG-FAC1 ROUNDED = (RG-N * RG-WK) - 1
              SIZE ERROR GO TO RGNY-NET-CL-ERROR.

           COMPUTE RG-FAC2 ROUNDED =
                ( 1 - 1 / (1 + 0.005417) ** RG-N-1 ) / 0.005417
              SIZE ERROR GO TO RGNY-NET-CL-ERROR.

           COMPUTE RG-NUM ROUNDED =
                ( RG-N * (RG-I - 0.005417) ) +
                ( (1 + RG-FAC1) * (RG-FAC2 - RG-A--N-1) )
              SIZE ERROR GO TO RGNY-NET-CL-ERROR.

           COMPUTE RG-DEN ROUNDED = RG-N * (RG-I - 0.005417)
              SIZE ERROR GO TO RGNY-NET-CL-ERROR.

           COMPUTE RG-WK ROUNDED =
                 RG-NUM / RG-DEN * SP-INSRATE(ILEV 1) * 0.001
              SIZE ERROR GO TO RGNY-NET-CL-ERROR.

       RGNY-NET-CL-END.
           COMPUTE IWA-INS ROUNDED =
               IWA-INS-CHARGEABLE * RG-WK / ( 1 - RG-WK )
              SIZE ERROR GO TO RGNY-NET-CL-ERROR.
           GO TO RGNY-NET-CL-EXIT.

       RGNY-NET-CL-ERROR.
           PERFORM COY-INS-ERROR.

       RGNY-NET-CL-EXIT.
           EXIT.

      *************************************************************
      *    REGENCY'S TN & KY NET PAYOFF LIFE
      *    TRUNCATED AND/OR FULL TERM COVERAGE
      *************************************************************
       RGTNKY-NET-CL  SECTION.
           MOVE 999         TO ELAPSED-YRTYPE.
           MOVE CIP-IN-DT   TO NUM-DATE.
           MOVE CIP-PMT-DT  TO SYS-DATE.
           PERFORM TIMALL.
           MOVE ELAPSED-REM TO RG-E.

           MOVE COP-INS-TERM(COPIX) TO RG-N.
           MOVE COP-TERM            TO RG-M.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              COMPUTE RG-I ROUNDED = WKS-RATE / 1200
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              COMPUTE RG-I ROUNDED = APR-APR / 1200.

           COMPUTE RG-A--M ROUNDED =
               (1 - 1 / (1 + RG-I) ** RG-M) / RG-I
                  SIZE ERROR GO TO RGTNKY-NET-CL-ERROR.
           COMPUTE RG-A--M-N ROUNDED =
               (1 - 1 / (1 + RG-I) ** (RG-M - RG-N)) / RG-I
                  SIZE ERROR GO TO RGTNKY-NET-CL-ERROR.

           COMPUTE RG-FAC1 ROUNDED = 1 + 12 * RG-I * RG-E / 365
                  SIZE ERROR GO TO RGTNKY-NET-CL-ERROR.

           IF SP-RG-TN-NET(ILEV)
              COMPUTE RG-FAC2 ROUNDED =
                  SP-INSRATE(ILEV 1) * RG-N / ( 600 * (RG-N + 1) )
                     SIZE ERROR GO TO RGTNKY-NET-CL-ERROR
              END-COMPUTE
           ELSE
              COMPUTE RG-FAC2 ROUNDED =
               SP-INSRATE(ILEV 1) / 100
                  SIZE ERROR GO TO RGTNKY-NET-CL-ERROR.

           COMPUTE RG-SPR ROUNDED =
               RG-FAC2 * RG-FAC1 * (RG-N - RG-A--M + RG-A--M-N)
                         / (RG-I * RG-A--M)
                  SIZE ERROR GO TO RGTNKY-NET-CL-ERROR.

       RGTNKY-NET-CL-END.
           IF SP-RG-TN-NET(ILEV)
              COMPUTE IWA-INS ROUNDED =
                  (1 + RG-I) * COP-REG-PAYMENT * RG-A--M
                              * RG-SPR / RG-FAC1
                  SIZE ERROR GO TO RGTNKY-NET-CL-ERROR
              END-COMPUTE
           ELSE
              COMPUTE IWA-INS =
                  RG-SPR * (1 + 0.5 * RG-I)
                    * COP-REG-PAYMENT * RG-A--M
                    / RG-FAC1
                  SIZE ERROR GO TO RGTNKY-NET-CL-ERROR.
           GO TO RGTNKY-NET-CL-EXIT.

       RGTNKY-NET-CL-ERROR.
           PERFORM COY-INS-ERROR.

       RGTNKY-NET-CL-EXIT.
           EXIT.

      *************************************************************
      *    INDIANA CREDIT LIFE & AH FORMULA
      *    FOR AMERICAN NATIONAL INSURANCE COMPANY    PACESETTER
      *************************************************************
       IN-NET-PACE  SECTION.
           MOVE 365                 TO ELAPSED-YRTYPE.
           MOVE CIP-IN-DT           TO NUM-DATE.
           MOVE CIP-PMT-DT          TO SYS-DATE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS        TO IN-OD.

           MOVE COP-INS-TERM(COPIX) TO IN-N.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              COMPUTE IN-I ROUNDED = WKS-RATE / 1200
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              COMPUTE IN-I ROUNDED = APR-APR / 1200.

           COMPUTE IN-A--N-I ROUNDED =
               (1 - 1 / (1 + IN-I) ** IN-N) / IN-I
                  SIZE ERROR GO TO IN-NET-PACE-ERROR.

           COMPUTE IN-OD30 ROUNDED = (IN-OD - 30) / 30.
           COMPUTE IN-ODF ROUNDED = 1 + IN-I * IN-OD30
               SIZE ERROR GO TO IN-NET-PACE-ERROR.

       IN-NET-PACE-END.
      *    - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TEST SOLVING FOR A&H INSURANCE PREMIUM
      *         THIS CALCULATION PER AMERICAN NATIONAL WRITE UP
      *         ASSUMES THAT A&H CAN'T BE WRITTEN WITH OUT
      *         CREDIT LIFE AND USES CREDIT LIFE RATES
      *    - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ILEV = 5
              COMPUTE IN-SP = IN-SP-FOR-AH
              COMPUTE IN-SP1 ROUNDED = SP-INSRATE(5 1) / 100 * IN-N
      *       COMPUTE IN-SP2 =
              COMPUTE IN-SP2 ROUNDED =
                  COP-PROCEEDS /
                  ( IN-A--N-I / IN-ODF
                      - (( IN-SP / 1200) * (IN-N + IN-OD30)
                          + SP-INSRATE(5 1) / 100
                        ) * IN-N
                  )
      *           SIZE ERROR GO TO IN-NET-PACE-ERROR
              COMPUTE IWA-INS ROUNDED = IN-SP1 * IN-SP2
              GO TO IN-NET-PACE-EXIT.

      *    - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ASSUME SOLVING FOR CL INSURANCE PREMIUM
      *         LEVELS OTHER THAN 5
      *    - - - - - - - - - - - - - - - - - - - - - - - - -
           COMPUTE IN-SP
                   IN-SP-FOR-AH = SP-INSRATE(ILEV 1).
           COMPUTE IN-SP1 ROUNDED =
               IN-SP * IN-N * (IN-N + IN-OD30) / 1200
               SIZE ERROR GO TO IN-NET-PACE-ERROR.

      *    SEE IF A&H IS BEING SOLVED FOR
      *    AND IF SO INCLUDED (A&H) SP-INSRATE(5 1) IN CALCULATION
           IF COP-INS-AH
              COMPUTE IN-SP2 ROUNDED =
      *       COMPUTE IN-SP2 =
                  COP-PROCEEDS /
                  ( IN-A--N-I / IN-ODF
                      - (( IN-SP / 1200) * (IN-N + IN-OD30)
                          + (SP-INSRATE(5 1) / 100)
                        ) * IN-N
                  )
                  SIZE ERROR GO TO IN-NET-PACE-ERROR
           ELSE
      *       COMPUTE IN-SP2 ROUNDED =
              COMPUTE IN-SP2 =
                  COP-PROCEEDS /
                  ( IN-A--N-I / IN-ODF
                      - (( IN-SP / 1200) * (IN-N + IN-OD30)
                        ) * IN-N
                  )
                  SIZE ERROR GO TO IN-NET-PACE-ERROR.

           COMPUTE IWA-INS ROUNDED = IN-SP1 * IN-SP2.
           GO TO IN-NET-PACE-EXIT.

       IN-NET-PACE-ERROR.
           PERFORM COY-INS-ERROR.

       IN-NET-PACE-EXIT.
           EXIT.


      ******************************************************************
      *    MAINE FINANCE NET PAYOFF
      *              INSURANCE FORMULA
      ******************************************************************
       MAINE-NET-CL  SECTION.

           MOVE COP-IN-DT TO NUM-DATE.
           MOVE COP-PMT-DT TO SYS-DATE.
           PERFORM TIM365.
           MOVE ELAPSED-DAYS TO RG-K-DAYS.

           MOVE COP-TERM TO RG-N.
           SUBTRACT 1 FROM COP-TERM GIVING RG-N-1.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              MOVE WKS-RATE TO IWA-RATE
              COMPUTE RG-A--N-1 ROUNDED =
                  (1 - 1 / (1 + IWA-RATE / 1200) ** RG-N-1)
                                           / (IWA-RATE / 1200)
                     SIZE ERROR GO TO MAINE-NET-CL-ERROR
              END-COMPUTE
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              IF APR-APR = 0
                 MOVE RG-N-1 TO RG-A--N-1
              ELSE
                 MOVE APR-APR TO IWA-RATE
                 COMPUTE RG-A--N-1 ROUNDED =
                     (1 - 1 / (1 + IWA-RATE / 1200) ** RG-N-1)
                                              / (IWA-RATE / 1200)
                     SIZE ERROR GO TO MAINE-NET-CL-ERROR.

           COMPUTE RG-I ROUNDED = IWA-RATE / 1200.

           COMPUTE RG-WK ROUNDED =
              1 / (1 + RG-N * SP-INSDISCNT(ILEV) * .01 / 24)
              SIZE ERROR GO TO MAINE-NET-CL-ERROR.

           COMPUTE RG-FAC1 ROUNDED =
                (1 + RG-A--N-1) / (1 + RG-K-DAYS * RG-I / 30)
              SIZE ERROR GO TO MAINE-NET-CL-ERROR.

           COMPUTE RG-FAC2 ROUNDED =
                ( (RG-N - 1) - RG-A--N-1 ) / RG-I
              SIZE ERROR GO TO MAINE-NET-CL-ERROR.

       MAINE-NET-CL-END.
           COMPUTE IWA-INS ROUNDED =
               SP-INSRATE(ILEV 1) * RG-WK
                  * ( RG-K-DAYS / 30 * RG-FAC1 + RG-FAC2
                    ) * COP-REG-PAYMENT
                  / 1000
              SIZE ERROR GO TO MAINE-NET-CL-ERROR.
           GO TO MAINE-NET-CL-EXIT.

       MAINE-NET-CL-ERROR.
           PERFORM COY-INS-ERROR.

       MAINE-NET-CL-EXIT.
           EXIT.

      ******************************************************************
      *    FREEDOM FINANCE BALLOON NET PAYOFF
      *              INSURANCE FORMULA
      ******************************************************************
       FREEDOM-NET-CL-BALLOON  SECTION.

           MOVE COP-TERM TO RG-N.
           SUBTRACT 1 FROM COP-TERM GIVING RG-N-1.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              MOVE WKS-RATE TO IWA-RATE
              COMPUTE IWA-LOAN-ANNUITY ROUNDED =
                  (1 - 1 / (1 + IWA-RATE / 1200) ** RG-N-1)
                                           / (IWA-RATE / 1200)
                     SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR
              END-COMPUTE
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              IF APR-APR = 0
                 MOVE RG-N-1 TO IWA-LOAN-ANNUITY
              ELSE
                 MOVE APR-APR TO IWA-RATE
                 COMPUTE IWA-LOAN-ANNUITY ROUNDED =
                     (1 - 1 / (1 + IWA-RATE / 1200) ** RG-N-1)
                                              / (IWA-RATE / 1200)
                     SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR.

           COMPUTE RG-I ROUNDED = IWA-RATE / 1200.

           MOVE IWA-LOAN-ANNUITY TO RG-A--N-1.

           COMPUTE RG-WK ROUNDED = (1 + RG-I) / (1 + RG-A--N-1)
              SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR.
           COMPUTE RG-FAC1 ROUNDED = (RG-N * RG-WK) - 1
              SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR.

           COMPUTE RG-FAC2 ROUNDED =
                ( 1 - 1 / (1 + 0.005417) ** RG-N-1 ) / 0.005417
              SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR.

           COMPUTE RG-NUM ROUNDED =
                ( RG-N * (RG-I - 0.005417) ) +
                ( (1 + RG-FAC1) * (RG-FAC2 - RG-A--N-1) )
              SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR.

           COMPUTE RG-DEN ROUNDED = RG-N * (RG-I - 0.005417)
              SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR.

           COMPUTE RG-WK ROUNDED =
                 RG-NUM / RG-DEN * SP-INSRATE(ILEV 1) * 0.001
              SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR.

       FREEDOM-NET-CL-BALLOON-END.
           COMPUTE IWA-INS ROUNDED =
               IWA-INS-CHARGEABLE * RG-WK / ( 1 - RG-WK )
              SIZE ERROR GO TO FREEDOM-NET-CL-BALLOON-ERROR.
           GO TO FREEDOM-NET-CL-BALLOON-EXIT.

       FREEDOM-NET-CL-BALLOON-ERROR.
           PERFORM COY-INS-ERROR.

       FREEDOM-NET-CL-BALLOON-EXIT.
           EXIT.


      *********************************************
       COW-SET-COP-INS  SECTION.
      *********************************************
           IF NOT SP-EXTEND-INSURANCE(ILEV)
              GO TO COW-TEST-MIN.
           MOVE COP-1ST-PMT-DAYS TO IWA-EXT-DAYS.
           IF COP-1ST-PMT-MTHS > 1
              COMPUTE IWA-EXT-DAYS = IWA-EXT-DAYS
                  + (COP-1ST-PMT-MTHS - 1) * 30.5
              SIZE ERROR GO TO COW-ERROR-EXIT.
           COMPUTE IWA-INS ROUNDED = IWA-INS + IWA-INS
               / COP-INS-TERM(COPIX) * IWA-EXT-DAYS / 30.5
           SIZE ERROR GO TO COW-ERROR-EXIT.

       COW-TEST-MIN.
           IF IWA-INS < SP-INSMINPREM(ILEV)
              MOVE SP-INSMINPREM(ILEV) TO IWA-INS.
           MOVE IWA-INS TO COP-INS-PREMIUM(COPIX).

      * TEMPORARY MAX UNTIL LOAN RECORD IS EXPANDED:
       COW-TEST-MAX.
           IF IWA-INS > 9999.99
      *       PERFORM COY-INS-ERROR
              MOVE 9999.99 TO IWA-INS
                              COP-INS-PREMIUM(COPIX)
                              CEP-INS-PREMIUM(COPIX).

           PERFORM COW-INS-TAX.
           GO TO COW-EXIT.

       COW-ERROR-EXIT.
           PERFORM COY-INS-ERROR.
           MOVE 0 TO COP-INS-TAX(COPIX).

       COW-EXIT.
           EXIT.

      ***************************************************************
      *    DETERMINE INSURANCE SALES TAX
      *
      *    ROUTINE ASSUMES 'ILEV' AND 'COPIX' HAVE BEEN SET
      *
      *    NOTE:
      *          CD-CCY-TYPE = " " -> INDICATES NO CITY SALES TAX
      *          CD-CCT-TYPE = " " -> INDICATES NO COUNTY SALES TAX
      ***************************************************************
       COW-INS-TAX SECTION.
           IF CD-CCY-TYPE = " " AND CD-CCT-TYPE = " "
              MOVE 0 TO COP-INS-TAX(COPIX)
              GO TO COW-INS-TAX-EXIT.
   
      * DETERMINE AMOUNT OF COUNTY TAX:
           IF CD-CCT-TYPE NOT = " "
              IF CD-CCT-INSTAXFRMLA(ILEV) = " "
                 COMPUTE WKS-CT-TAX ROUNDED =
                     COP-INS-PREMIUM(COPIX)
                         * CD-CCT-INSTAXAMT(ILEV) * 0.01
              ELSE
                 MOVE CD-CCT-INSTAXAMT(ILEV) TO WKS-CT-TAX
              END-IF
           ELSE
              MOVE 0 TO WKS-CT-TAX.
   
      * DETERMINE AMOUNT OF CITY TAX:
           IF CD-CCY-TYPE NOT = " "
              IF CD-CCY-INSTAXFRMLA(ILEV) = " "
                 COMPUTE WKS-CY-TAX ROUNDED =
                     COP-INS-PREMIUM(COPIX)
                         * CD-CCY-INSTAXAMT(ILEV) * 0.01
              ELSE
                 MOVE CD-CCY-INSTAXAMT(ILEV) TO WKS-CY-TAX
              END-IF
           ELSE
              MOVE 0 TO WKS-CY-TAX.
   
      * DETERMINE IF THE COUNTY TAX AND CITY TAX SHOULD BE COMBINED:
           IF CD-CCY-TYPE NOT = " "
              IF CD-CCY-BOTH-TAX-FG = "Y"
                 COMPUTE COP-INS-TAX(COPIX) =
                     WKS-CT-TAX + WKS-CY-TAX
              ELSE
                 IF WKS-CT-TAX > WKS-CY-TAX
                    MOVE WKS-CT-TAX TO COP-INS-TAX(COPIX)
                 ELSE
                    MOVE WKS-CY-TAX TO COP-INS-TAX(COPIX)
                 END-IF
              END-IF
           ELSE
              MOVE WKS-CT-TAX TO COP-INS-TAX(COPIX).
   
       COW-INS-TAX-EXIT.
           EXIT.


      *********************************************
       COX-INS-ERROR  SECTION.
      *********************************************
           PERFORM COZ-INS-ERROR.
           MOVE 0 TO COP-INS-PREMIUM(COPIX).

       COX-EXIT.
           EXIT.


      *********************************************
       COY-INS-ERROR  SECTION.
      *********************************************
           PERFORM COZ-INS-ERROR.
           MOVE WKS-ALL-NINES TO COP-INS-PREMIUM(COPIX).

       COY-EXIT.
           EXIT.


      *********************************************
       COZ-INS-ERROR  SECTION.
      *********************************************
           IF COPIX = 1
             MOVE "F" TO COP-RETURN-CODE
           ELSE IF COPIX = 2
             MOVE "G" TO COP-RETURN-CODE
           ELSE IF COPIX = 3
             MOVE "H" TO COP-RETURN-CODE
           ELSE IF COPIX = 4
             MOVE "Q" TO COP-RETURN-CODE.

       COZ-EXIT.
           EXIT.

      *******************************************************
      *    MERCURY'S VIRGINIA PRECOMPUTE INSURANCE FORMULA
      *******************************************************
       CVA5-PER-1000-PC  SECTION.
           MOVE COP-TERM TO CVA-N.
           COMPUTE CVA-SP ROUNDED =
                  SP-INSRATE(ILEV 1)
                      / (1 + SP-INSDISCNT(ILEV) / 2400 * CVA-N)
              SIZE ERROR GO TO CVA5-PER-1000-PC-ERROR.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              MOVE WKS-RATE TO IWA-RATE
              MOVE WKS-ANNUITY TO IWA-LOAN-ANNUITY
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              MOVE APR-APR TO IWA-RATE
              COMPUTE IWA-LOAN-ANNUITY ROUNDED =
                  (1 - 1 / (1 + IWA-RATE / 1200) ** CVA-N)
                                           / (IWA-RATE / 1200)
                  SIZE ERROR GO TO CVA5-PER-1000-PC-ERROR.

           COMPUTE CVA-I ROUNDED = IWA-RATE / 1200.
           MOVE IWA-LOAN-ANNUITY TO CVA-A--N.

           COMPUTE CVA-LP ROUNDED =
              CVA-N * (CVA-N + 1)
                  * (6 * CVA-N * CVA-N - 3 * CVA-N * (CVA-N - 1)).

           COMPUTE CVA-LP ROUNDED =
              (
                CVA-LP -
                      2 * (CVA-N - CVA-A--N) * (CVA-N + 2)
                          * (CVA-N + 1) * CVA-N
              ) /
                 (
                   6 * CVA-A--N * (CVA-N + 1) * (CVA-N)
                 ).

       CVA5-PER-1000-PC-END.
           COMPUTE IWA-INS ROUNDED =
              IWA-INS-CHARGEABLE * CVA-SP * CVA-LP * .001.
           GO TO CVA5-PER-1000-PC-EXIT.

       CVA5-PER-1000-PC-ERROR.
           PERFORM COY-INS-ERROR.

       CVA5-PER-1000-PC-EXIT.
           EXIT.

      *************************************************************
      *    MERCURY'S VIRGINIA INTEREST BEARING INSURANCE FORMULA
      *               AND ALABAMA SINGLE PREMIUM NET PAYOFF
      *************************************************************
       CVA6-PER-1000-IB  SECTION.
           MOVE COP-TERM TO CVA-N.
           IF SP-AL-NETPAYOFF(ILEV)
              MOVE SP-INSRATE(ILEV 1) TO CVA-SP3
           ELSE
              COMPUTE CVA-SP3 =
                SP-INSRATE(ILEV 1) * (CVA-N + 1)
                   / (20 * (1 + SP-INSDISCNT(ILEV) * CVA-N / 2400))
                      * (12 / CVA-N)
              SIZE ERROR GO TO CVA6-PER-1000-IB-ERROR.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              MOVE WKS-RATE TO IWA-RATE
              MOVE WKS-ANNUITY TO IWA-LOAN-ANNUITY
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              IF APR-APR = 0
                 MOVE CVA-N TO IWA-LOAN-ANNUITY
              ELSE
                 MOVE APR-APR TO IWA-RATE
                 COMPUTE IWA-LOAN-ANNUITY ROUNDED =
                     (1 - 1 / (1 + IWA-RATE / 1200) ** CVA-N)
                                              / (IWA-RATE / 1200)
                     SIZE ERROR GO TO CVA6-PER-1000-IB-ERROR.

           COMPUTE CVA-I ROUNDED = IWA-RATE / 1200.
           MOVE IWA-LOAN-ANNUITY TO CVA-A--N.

           COMPUTE CVA-WK ROUNDED =
              CVA-SP3 * CVA-N / 600
                  * (CVA-N - CVA-A--N)
                        / ((CVA-N + 1) * CVA-I * CVA-A--N).

       CVA6-PER-1000-IB-END.
           COMPUTE IWA-INS ROUNDED = IWA-INS-CHARGEABLE * CVA-WK.
           GO TO CVA6-PER-1000-IB-EXIT.

       CVA6-PER-1000-IB-ERROR.
           PERFORM COY-INS-ERROR.

       CVA6-PER-1000-IB-EXIT.
           EXIT.

      *************************************************************
      *    MULLEN'S CALIFORNIA CL NET PAYOFF
      *************************************************************
       CVCA-NET-POFF-MULL  SECTION.
           COMPUTE CVM-J ROUNDED = 4.2 / 1200.
           MOVE COP-TERM TO CVM-N.
           MOVE COP-INS-TERM(COPIX) TO CVM-P.

           COMPUTE CVM-A--P-J ROUNDED =
                  (1 - 1 / (1 + CVM-J) ** CVM-P) / CVM-J
                  SIZE ERROR GO TO CVCA-NET-POFF-MULL-ERROR.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              COMPUTE CVM-I ROUNDED = WKS-RATE / 1200
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              COMPUTE CVM-I ROUNDED = APR-APR / 1200.
           COMPUTE CVM-A--P-I ROUNDED =
               (1 - 1 / (1 + CVM-I) ** CVM-P) / CVM-I
                  SIZE ERROR GO TO CVCA-NET-POFF-MULL-ERROR.

           COMPUTE CVM-WK ROUNDED =
              ( 1 / CVM-I * (1 + CVM-J)
                     * (CVM-A--P-J
                           + (CVM-J * CVM-A--P-J - CVM-I * CVM-A--P-I)
                              / (1 + CVM-I) ** (CVM-N - CVM-P)
                              / (CVM-I - CVM-J)
                       )
              ).

       CVCA-NET-POFF-MULL-END.
           COMPUTE IWA-INS ROUNDED =
      *          IWA-INS-CHARGEABLE * SP-INSRATE(ILEV 1)
                 COP-REG-PAYMENT * SP-INSRATE(ILEV 1)
                                           * .001 * CVM-WK.
           GO TO CVCA-NET-POFF-MULL-EXIT.

       CVCA-NET-POFF-MULL-ERROR.
           PERFORM COY-INS-ERROR.

       CVCA-NET-POFF-MULL-EXIT.
           EXIT.

      *************************************************************
      *    MERCURY'S CALIFORNIA CL NET PAYOFF
      *************************************************************
       CVCA-NETPAYOFF-5A  SECTION.
           MOVE 1.0035 TO CVA-D.
           MOVE COP-TERM TO CVA-M.
           MOVE COP-INS-TERM(COPIX) TO CVA-N.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              COMPUTE CVA-I ROUNDED = WKS-RATE / 1200
              MOVE WKS-ANNUITY TO CVA-A--N
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              COMPUTE CVA-I ROUNDED = APR-APR / 1200
              COMPUTE CVA-A--N ROUNDED =
                  (1 - 1 / (1 + CVA-I) ** CVA-M) / CVA-I
                  SIZE ERROR GO TO CVCA-NETPAYOFF-5A-ERROR.

           COMPUTE CVA-V ROUNDED = 1 / (1 + CVA-I).

           COMPUTE CVA-WK ROUNDED =
              ( 1 / (CVA-I * CVA-A--N)
                      * ( ( ( CVA-D - ((1 / CVA-D) ** (CVA-N - 1))
                            ) / (0.042 / 12)
                          ) -
                            ( CVA-V ** CVA-M
                                  - CVA-V ** (CVA-M - CVA-N)
                                                  / CVA-D ** CVA-N
                            ) / ( 1 - 1 / (CVA-D * CVA-V))
                        )
              ).

       CVCA-NETPAYOFF-5A-END.
           COMPUTE IWA-INS ROUNDED =
                 IWA-INS-CHARGEABLE * SP-INSRATE(ILEV 1)
                                           * .001 * CVA-WK.
           GO TO CVCA-NETPAYOFF-5A-EXIT.

       CVCA-NETPAYOFF-5A-ERROR.
           PERFORM COY-INS-ERROR.

       CVCA-NETPAYOFF-5A-EXIT.
           EXIT.

      *************************************************************
      *    MULLEN'S CALIFORNIA CL TRUNCATED NET
      *************************************************************
       CA-TRUNC-NET-MULL SECTION.

           MOVE SP-INSRATE(ILEV, 1) TO WKS-INSRATE-1000.

           MOVE COP-IN-DT  TO NUM-DATE.
           MOVE COP-PMT-DT TO SYS-DATE.
           PERFORM TIM365.
           COMPUTE CA-E ROUNDED = ELAPSED-DAYS / 30 - 1.

           MOVE COP-TERM            TO CA-N.
           MOVE COP-INS-TERM(COPIX) TO CA-T.

           PERFORM XBA-CALCULATE-SIMPLE-INTEREST.
           COMPUTE IN-I ROUNDED = APR-APR / 1200.

           COMPUTE CA-A--N-I ROUNDED =
               (1 - 1 / (1 + CA-I) ** CA-N) / CA-I.

           COMPUTE CA-A--N-T-I ROUNDED =
               (1 - 1 / (1 + CA-I) ** (CA-N - CA-T)) / CA-I.

           COMPUTE CA-B ROUNDED =
              (1 - (1 / 1.0035 ** CA-T)) / 0.0035.

           COMPUTE CA-W ROUNDED =
              CA-B - CA-A--N-I + CA-A--N-T-I
                   - 0.0035 * CA-B * CA-A--N-T-I.

           COMPUTE CA-S ROUNDED =
                (1 + CA-I) / (1 + CA-I + CA-I * CA-E).

           COMPUTE CA-G ROUNDED =
                CA-A--N-I * (CA-S * CA-E + CA-S - 1).

           COMPUTE CA-K ROUNDED =
                WKS-INSRATE-1000 * 0.001
                   * ((1.0035 * CA-W) / (CA-I - 0.0035) + CA-G).

       CA-TRUNC-NET-MULL-END.
           COMPUTE IWA-INS ROUNDED = COP-REG-PAYMENT * CA-K
              SIZE ERROR GO TO CA-TRUNC-NET-MULL-ERROR.

           GO TO CA-TRUNC-NET-MULL-EXIT.

       CA-TRUNC-NET-MULL-ERROR.
           PERFORM COY-INS-ERROR.

       CA-TRUNC-NET-MULL-EXIT.
           EXIT.

      *************************************************************
      *    WORLD ALABAMA NET PAYOFF PLUS 1 PAYMENT
      *************************************************************
       AL-NET-PLUS1-WRLD SECTION.
           MOVE SP-INSRATE(ILEV, 1) TO WKS-INSRATE-1000.

           MOVE COP-TERM            TO AL-N.
           MOVE COP-INS-TERM(COPIX) TO AL-T.

           IF COP-OVR-RATE-IS-APR OR SP-CAL-SIMPLE(CTBL)
              COMPUTE AL-I ROUNDED = WKS-RATE / 1200
           ELSE
              PERFORM XBA-CALCULATE-SIMPLE-INTEREST
              COMPUTE AL-I ROUNDED = APR-APR / 1200.

           COMPUTE AL-A--N-I ROUNDED =
               (1 - 1 / (1 + AL-I) ** AL-N) / AL-I.

           COMPUTE AL-A--N-T-I ROUNDED =
               (1 - 1 / (1 + AL-I) ** (AL-N - AL-T)) / AL-I.

       AL-NET-PLUS1-WRLD-END.
           COMPUTE IWA-INS ROUNDED =
               WKS-INSRATE-1000 * .001 * COP-REG-PAYMENT *
                ( ( (AL-T + AL-A--N-T-I - AL-A--N-I) / AL-I ) + AL-T )

               SIZE ERROR GO TO AL-NET-PLUS1-WRLD-ERROR.

           GO TO AL-NET-PLUS1-WRLD-EXIT.

       AL-NET-PLUS1-WRLD-ERROR.
           PERFORM COY-INS-ERROR.

       AL-NET-PLUS1-WRLD-EXIT.
           EXIT.

      ********************************************
      *    MAINTENANCE FEE CALCULATIONS
      ********************************************
       CPA-CALC-MAINT-FEES  SECTION.
      *  IN THIS SECTION MAKE SURE THAT ALL MAINTENANCE FEES ARE
      *  DIVISABLE BY THE TERM OF THE CONTRACT

           IF COP-MAINT-FEES-ARE-INPUT
              GO TO CPA-ROUND-PAYMENT.

           IF SP-MFFRMLA-01 OR SP-MFFRMLA-03 OR SP-MFFRMLA-04
              PERFORM CQA-MAINT-FORMULA-01-03-04
              GO TO CPA-ROUND-PAYMENT.

           IF SP-MFFRMLA-02
              MULTIPLY COP-TERM BY SP-MFRATE GIVING COP-MAINT-FEES
              GO TO CPA-ROUND-PAYMENT.

           IF SP-MFFRMLA-05
              COMPUTE COP-MAINT-FEES ROUNDED =
                  ( COP-INT-CHARGE + COP-SERVICE-CHG )
                  * SP-MFRATE / 100
      *------------------------------------------------------------------
      *  CHANGE MADE TO MAKE DL WORK LIKE BALLOONS.
      *    SPR FLAG MUST BE SET SO AS TO NOT AFFECT EXISTING CANNED LOANS
      *------------------------------------------------------------------
              IF SP-ROUND-MFEE-YES
                 DIVIDE COP-MAINT-FEES BY COP-TERM
                        GIVING MXMF-WORKER REMAINDER MXMF-REMAINDER
                 IF MXMF-REMAINDER NOT = 0.00
                    COMPUTE MXMF-OFFSET =
                         (COP-TERM / 100) - MXMF-REMAINDER
                    ADD MXMF-OFFSET TO COP-MAINT-FEES
                 END-IF
              END-IF
              GO TO CPA-ROUND-PAYMENT.

           GO TO CPA-EXIT.

       CPA-ROUND-PAYMENT.
           IF COP-MAINT-FEES < SP-MFMIN
              GO TO CPA-LESS-THAN-MIN.
           IF (COP-MAINT-FEES > SP-MFMAX) AND
                      (SP-MFMAX NOT = 9999.99)
              GO TO CPA-GREATER-THAN-MAX.
           GO TO CPA-EDITS-ARE-PASSED.

       CPA-LESS-THAN-MIN.
           MOVE SP-MFMIN TO COP-MAINT-FEES.

       CPA-EDITS-ARE-PASSED.
           DIVIDE COP-MAINT-FEES BY COP-TERM
                              GIVING WKS-MTHLY-MAINT-FEES.
           IF SP-MFFRMLA-05
              GO TO CPA-EXIT.

           COMPUTE WKS-TEST-AMOUNT = WKS-MTHLY-MAINT-FEES * COP-TERM.
           IF WKS-TEST-AMOUNT NOT = COP-MAINT-FEES
              ADD .01 TO WKS-MTHLY-MAINT-FEES.
           GO TO CPA-EXTENT-MONTHLY-FEES.

       CPA-GREATER-THAN-MAX.
           MOVE SP-MFMAX TO COP-MAINT-FEES.
           DIVIDE COP-MAINT-FEES BY COP-TERM
                             GIVING WKS-MTHLY-MAINT-FEES.

       CPA-EXTENT-MONTHLY-FEES.
           MULTIPLY COP-TERM BY WKS-MTHLY-MAINT-FEES
                             GIVING COP-MAINT-FEES.

       CPA-EXIT.
           EXIT.


      * MFFRMLA = 03 MERCURY FINANCE 10/14/99  STATE AUDITOR
      * AMOUNT TO TEST = TOTAL AMT OF MONEY SCHEDULED TO BE PAID
      * BY THE BORROWER TO A REGISTRANT TO REPAY A LOAN, INCLUDING
      * PRINCIPAL AND ANY INTEREST PRECOMPUTED AND DEDUCTED IN ADVANCE

      *********************************************
       CQA-MAINT-FORMULA-01-03-04  SECTION.
      *********************************************
           MOVE 0 TO COP-MAINT-FEES.

           IF NOT WKS-CQA-MAINT-FEE-OK
              GO TO CQA-EXIT.

           COMPUTE WKS-TEST-AMOUNT = WKS-REG-PAYMENT
                                      - WKS-MTHLY-MAINT-FEES.
           IF WKS-TEST-AMOUNT < 15
              MOVE 0 TO COP-MAINT-FEES
              GO TO CQA-EXIT.

           IF SP-MFFRMLA-01 OR SP-MFFRMLA-04
              MOVE COP-TOTAL-NOTE TO WKS-TEST-AMOUNT
           ELSE
      * SP-MFFRMLA-03   MERCURY
              MOVE COP-TOT-REPYBLE TO WKS-TEST-AMOUNT.

      * CHANGE TO AMOUNTS, 6/11/96, E#124
           IF WKS-TEST-AMOUNT < 100
              MOVE 2.50 TO WKS-MTHLY-MAINT-FEES
           ELSE
              IF WKS-TEST-AMOUNT NOT > 750
                 MOVE 3.50 TO WKS-MTHLY-MAINT-FEES
              ELSE
                 IF WKS-TEST-AMOUNT NOT > 1250
                    MOVE 3.00 TO WKS-MTHLY-MAINT-FEES
                 ELSE
                    MOVE 2.50 TO WKS-MTHLY-MAINT-FEES.

           MULTIPLY COP-TERM BY WKS-MTHLY-MAINT-FEES
                                           GIVING COP-MAINT-FEES.
           IF COP-REG-PAYMENT-IS-INPUT
              COMPUTE WKS-TEST-AMOUNT = WKS-REG-PAYMENT
                                              - WKS-MTHLY-MAINT-FEES
              IF WKS-TEST-AMOUNT < 15
                 MOVE 0 TO COP-MAINT-FEES.

       CQA-EXIT.
           EXIT.

      *********************************************
      *    CALCULATE THE SERVICE CHARGE:
      *********************************************
       CSA-CALC-SERVICE-CHARGES  SECTION.
           IF COP-SERVICE-CHG-IS-INPUT
              IF SP-SCFRMLA = 12 OR 13 OR 54
                 IF COP-SERVICE-CHG < COP-STATE-WORKER
                    MOVE COP-SERVICE-CHG TO COP-STATE-WORKER.
           IF COP-SERVICE-CHG-IS-INPUT
               AND (NOT SP-SCFRMLA-POINTS)
             GO TO CSA-CHECK-MIN-MAX.

           ADD 1 SP-SCFRMLA GIVING WKS-GO-TO.
           GO TO CSA-EXIT
                 CSA-POINTS
                 CSA-50-DOLLARS-2-35-38
                 CSA-4-PERCENT-3
                 CSA-PERCENT-4-16-27-52-59
                 CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58
                 CSA-FLAT-PERCENT-6-56-62
                 CSA-NEW-MONEY-PERCENT-7-25-26
                 CSA-POINTS
                 CSA-TOTREPAY-9
                 CSA-TOTNOTE-10
                 CSA-TOTNOTE-11
                 CSA-GEORGIA-12-13-54
                 CSA-GEORGIA-12-13-54
                 CSA-MICHIGAN-14-21-22
                 CSA-TENNESSEE-15-17-60-61
                 CSA-PERCENT-4-16-27-52-59
                 CSA-TENNESSEE-15-17-60-61
                 CSA-SCAROLINA-18-42
                 CSA-4-PERCENT-19-30
                 CSA-PERCENT-20-46-51
                 CSA-MICHIGAN-14-21-22
                 CSA-MICHIGAN-14-21-22
                 CSA-NC-FLAT-23-43-44-45-50-55-57
                 CSA-NB-SPLIT-24
                 CSA-NEW-MONEY-PERCENT-7-25-26
                 CSA-NEW-MONEY-PERCENT-7-25-26
                 CSA-PERCENT-4-16-27-52-59
                 CSA-POINTS
                 CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58
                 CSA-4-PERCENT-19-30
                 CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58
                 CSA-POINTS
                 CSA-POINTS
                 CSA-POINTS
                 CSA-50-DOLLARS-2-35-38
                 CSA-POINTS
                 CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58
                 CSA-50-DOLLARS-2-35-38
                 CSA-SPLIT-RATE-39-40
                 CSA-SPLIT-RATE-39-40
                 CSA-POINTS
                 CSA-SCAROLINA-18-42
                 CSA-NC-FLAT-23-43-44-45-50-55-57
                 CSA-NC-FLAT-23-43-44-45-50-55-57
                 CSA-NC-FLAT-23-43-44-45-50-55-57
                 CSA-PERCENT-20-46-51
                 CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58
                 CSA-POINTS
                 CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58
                 CSA-NC-FLAT-23-43-44-45-50-55-57
                 CSA-PERCENT-20-46-51
                 CSA-PERCENT-4-16-27-52-59
                 CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58
                 CSA-GEORGIA-12-13-54
                 CSA-NC-FLAT-23-43-44-45-50-55-57
                 CSA-FLAT-PERCENT-6-56-62
                 CSA-NC-FLAT-23-43-44-45-50-55-57
                 CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58
                 CSA-PERCENT-4-16-27-52-59
                 CSA-TENNESSEE-15-17-60-61
                 CSA-TENNESSEE-15-17-60-61
                 CSA-FLAT-PERCENT-6-56-62
                 CSA-SPLIT-RATE-PCT-63
             DEPENDING WKS-GO-TO.
           GO TO CSA-ERROR-EXIT.

      * POINTS FOR SCFRMLA #'S 1,8,28,32,33,34,36,41,48.
       CSA-POINTS.
           PERFORM CTA-POINTS.
           GO TO CSA-EXIT.

       CSA-50-DOLLARS-2-35-38.
           IF SP-SCFRMLA = 35 OR 38
              PERFORM CTD-50-UNIT
           ELSE
              PERFORM CTD-50-DOLLARS.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-4-PERCENT-3.
           MOVE COP-TOTAL-NOTE TO COP-SERVICE-CHG.
           PERFORM CTG-4-PERCENT.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-PERCENT-4-16-27-52-59.
           PERFORM CTJ-PERCENT.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-FLAT-AMOUNT-5-29-31-37-47-49-53-58.
           PERFORM CTM-FLAT-AMOUNT.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-FLAT-PERCENT-6-56-62.
           PERFORM CTP-FLAT-PERCENT.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-NEW-MONEY-PERCENT-7-25-26.
           PERFORM CTS-NEW-MONEY-PERCENT.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-TOTREPAY-9.
           PERFORM CTR-TOTREPAY.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-TOTNOTE-10.
           PERFORM CTR-TOTNOTE.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-TOTNOTE-11.
           PERFORM CTG-PERCENT.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-GEORGIA-12-13-54.
           PERFORM CTU-GEORGIA-12-13-54.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-MICHIGAN-14-21-22.
           PERFORM CTW-MICHIGAN-14-21-22.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-TENNESSEE-15-17-60-61.
           PERFORM CTX-TENNESSEE-15-17-60-61.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-SCAROLINA-18-42.
           PERFORM CTY-SCAROLINA-18-42.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-4-PERCENT-19-30.
           COMPUTE COP-SERVICE-CHG = COP-TOTAL-NOTE + COP-1ST-PY-EXT.
           IF SP-SCFRMLA = 30
              MOVE COP-AMT-FINANCD TO COP-SERVICE-CHG.
           PERFORM CTG-4-PERCENT.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-PERCENT-20-46-51.
      * FORMULA 51 JUST LIKE 20 EXCEPT THE PRINCIPAL CANNOT INCLUDE FEES
      * THAT ARE FLAGGED AS A FINANCE CHARGE, WORLD PR# 746
           IF SP-SCFRMLA = 51
              COMPUTE COP-SERVICE-CHG ROUNDED =
                       (COP-AMT-FINANCD - COP-GL-AMOUNT-TOTAL-FINCHG)
                           * .01 * SP-SCRATE
                 SIZE ERROR GO TO CSA-ERROR-EXIT
           ELSE
              COMPUTE COP-SERVICE-CHG ROUNDED =
                           COP-AMT-FINANCD * .01 * SP-SCRATE
                 SIZE ERROR GO TO CSA-ERROR-EXIT.

      * LENDMARK - NEW SCFRMLA EXACTLY LIKE 20 EXCEPT ONLY ON ACCOUNTS
      *            WITH SOURCE CODE OF THESE, AND PRIOR STATE,CLASS, AND
      *            SUB CLASS MUST MATCH, OR NO CHARGE
           IF SP-SCFRMLA = 46
              IF NOT (COP-SOURCE-CD = "PB" OR "RB" OR "RN" OR "SC")
                 MOVE 0 TO COP-SERVICE-CHG
                 GO TO CSA-EXIT
              ELSE
      * "SC" ONLY THE STATE MUST MATCH
                 IF COP-SOURCE-CD = "SC"
                    IF SP-ORGST NOT = CIP-PRIOR-LOAN-STATE
                       MOVE 0 TO COP-SERVICE-CHG
                       GO TO CSA-EXIT
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF SP-ORGST NOT = CIP-PRIOR-LOAN-STATE OR
                        SP-SPRCLASS NOT = CIP-PRIOR-LOAN-CLASS OR
                         SP-SUBCLASS NOT = CIP-PRIOR-SUB-CLASS
                            MOVE 0 TO COP-SERVICE-CHG
                            GO TO CSA-EXIT.


           GO TO CSA-CHECK-MIN-MAX.
       CSA-NC-FLAT-23-43-44-45-50-55-57.
           PERFORM CTM-FLAT-AMOUNT.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-NB-SPLIT-24.
           PERFORM CTM-SPLIT-24.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-SPLIT-RATE-39-40.
           PERFORM CTM-SPLIT-RATE.
           GO TO CSA-CHECK-MIN-MAX.
       CSA-SPLIT-RATE-PCT-63.
      D    STOP MESS.
      * COP-RESTRICT-SERCHG IS PRIOR LOAN SERCHG RESTRICTION FROM WISCHG
           EVALUATE TRUE
           WHEN COP-AMT-FINANCD NOT >  500
                COMPUTE COP-SERVICE-CHG ROUNDED =
                                      COP-AMT-FINANCD * .10
           WHEN COP-AMT-FINANCD NOT >  1000
                COMPUTE COP-SERVICE-CHG ROUNDED =
                                      COP-AMT-FINANCD * .075
           WHEN OTHER
                COMPUTE COP-SERVICE-CHG ROUNDED =
                     COP-AMT-FINANCD * .05
                     IF COP-SERVICE-CHG > 175.00
                        MOVE 175.00 TO COP-SERVICE-CHG
                     END-IF
           END-EVALUATE.

      * COP-RESTRICT-SERCHG WILL ONLY HAVE A VALUE IF WISCHG FOUND IT
      * RESTRICTED BASED ON PRIOR LOAN
           IF COP-RESTRICT-SERCHG > COP-SERVICE-CHG
              MOVE 0 TO COP-SERVICE-CHG
           ELSE
              COMPUTE COP-SERVICE-CHG = COP-SERVICE-CHG
                                     - COP-RESTRICT-SERCHG.

           GO TO CSA-CHECK-MIN-MAX.


       CSA-CHECK-MIN-MAX.
           IF COP-SERVICE-CHG > SP-SCMAX
              MOVE SP-SCMAX TO COP-SERVICE-CHG
           ELSE
              IF COP-SERVICE-CHG < SP-SCMIN
                 MOVE SP-SCMIN TO COP-SERVICE-CHG.
           GO TO CSA-EXIT.

       CSA-ERROR-EXIT.
           MOVE "J" TO COP-RETURN-CODE.
       CSA-EXIT.
           EXIT.


      *********************************************
       CTA-POINTS  SECTION.
      *********************************************
      * FRMLA 33, IF SVCCHG-SW = "0", NO SERVICE CHARGE, NO ORIGINATION
      * FRMLA 33, IF SVCCHG-SW = "2", NO SERVICE CHARGE, BUT TAKE
      *                                     ORIGINATION
           IF (SP-SCFRMLA = 32 OR 33)
              IF WKS-CTJ-SVCCHG-SW = "0"
                 MOVE 0 TO COP-SERVICE-CHG
                           COP-STATE-WORKER
                 GO TO CTA-EXIT.

           SUBTRACT COP-INT-CHARGE FROM COP-TOTAL-NOTE
                                     GIVING WKS-PRINCIPAL.

      *-----------------------------------------------------------------------
      * FRMLA 48, TN REAL ESTATE LOANS REGENCY PR#2880
      *    USE RESTRICT-ADJ TO DETERMINE IF NEW MONIES ARE INCLUDED
      *    (NEW MONIES EXCLUDED IF PREV LOANS SPR HAD SAME SVCCHG)
      *-----------------------------------------------------------------------
           IF SP-SCFRMLA = 48
              IF COP-RESTRICT-ADJ = 50.00
                 SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM WKS-PRINCIPAL
              END-IF
              MOVE COP-POINTS TO SAVE-POINTS
                                 MAX-POINTS
              IF WKS-PRINCIPAL <= 30000.00
                 IF MAX-POINTS > 5.0
                    MOVE 5.0 TO MAX-POINTS
                 END-IF
              ELSE
                 IF MAX-POINTS > 3.0
                    MOVE 3.0 TO MAX-POINTS
                    COMPUTE MAX-SVCCHG ROUNDED =
                          MAX-POINTS * WKS-PRINCIPAL * .01
                 END-IF
              END-IF
              MOVE "C" TO COP-SERVICE-CHG-SW
              COMPUTE COP-SERVICE-CHG ROUNDED =
                          MAX-POINTS * WKS-PRINCIPAL * .01
                SIZE ERROR MOVE "J" TO COP-RETURN-CODE
                           MOVE WKS-ALL-NINES TO COP-SERVICE-CHG
              END-COMPUTE

              IF COP-SERVICE-CHG NOT = WKS-ALL-NINES
                 IF WKS-PRINCIPAL > 30000.00
                    IF COP-POINTS > 3.0
                       IF MAX-SVCCHG > 1500
                          MOVE MAX-SVCCHG TO COP-SERVICE-CHG
                       ELSE
                          MOVE 1500 TO COP-SERVICE-CHG
                       END-IF
                    END-IF
                 END-IF
              END-IF

              MOVE SAVE-POINTS TO COP-POINTS
              GO TO CTA-EXIT.

           IF SP-SCFRMLA = 8 OR 32 OR 36 OR 41
              SUBTRACT COP-INT-CHARGE COP-SERVICE-CHG
                 FROM COP-TOTAL-NOTE GIVING WKS-PRINCIPAL
              IF SP-SCFRMLA = 36 OR 41
                 IF WKS-CTS-SVCCHG-OK
                    SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM WKS-PRINCIPAL.
           IF SP-SCFRMLA = 28
              MOVE COP-PROCEEDS TO WKS-PRINCIPAL.
           IF SP-SCFRMLA = 34
              MOVE COP-NEW-LOAN-PROCEEDS TO WKS-PRINCIPAL.

           IF COP-SERVICE-CHG-IS-INPUT
              PERFORM CTA-CALC-POINTS
              IF NOT COP-UNKNOWN-ERROR
                 GO TO CTA-EXIT
              END-IF
           ELSE
              PERFORM CTA-CALC-SERVICE-CHG
              IF NOT COP-UNKNOWN-ERROR
                 GO TO CTA-EXIT
              END-IF
              IF SP-SCFRMLA = 33
                 COMPUTE WS-WORKER = WKS-PRINCIPAL - COP-STATE-WORKER
                 EVALUATE TRUE
                   WHEN WS-WORKER <  500 MOVE 15.00 TO COP-STATE-WORKER
                   WHEN WS-WORKER < 1000 MOVE 30.00 TO COP-STATE-WORKER
                   WHEN WS-WORKER < 2000 MOVE 100.00 TO COP-STATE-WORKER
                   WHEN WS-WORKER < 5000 MOVE 200.00 TO COP-STATE-WORKER
                   WHEN OTHER  COMPUTE COP-STATE-WORKER ROUNDED =
                                  (WKS-PRINCIPAL - COP-STATE-WORKER)
                                                                  * .01
                               IF COP-STATE-WORKER < 250.00
                                  MOVE 250.00 TO COP-STATE-WORKER
                               END-IF
                 END-EVALUATE
      * "2" = ELAPSED-DAYS > 90 < 366, NO SERVICE CHARGE, BUT TAKE ORIGINATION
                 IF WKS-CTJ-SVCCHG-SW = "2"
                    MOVE 0 TO COP-SERVICE-CHG
                 END-IF
                 ADD COP-STATE-WORKER TO COP-SERVICE-CHG.

           IF COP-SERVICE-CHG-IS-CALCD AND
              POINTS-TEST-MAX-FG = "N"
              GO TO CTA-EXIT.

           IF COP-SERVICE-CHG > SP-SCMAX
              MOVE SP-SCMAX TO COP-SERVICE-CHG
              MOVE "I" TO COP-SERVICE-CHG-SW
              PERFORM CTA-CALC-POINTS
           ELSE
              IF COP-SERVICE-CHG < SP-SCMIN
                 MOVE SP-SCMIN TO COP-SERVICE-CHG
                 MOVE "I" TO COP-SERVICE-CHG-SW
                 PERFORM CTA-CALC-POINTS.
           GO TO CTA-EXIT.

       CTA-CALC-POINTS.
           MOVE "C" TO COP-POINTS-SW.
           COMPUTE COP-POINTS ROUNDED =
             (COP-SERVICE-CHG - COP-STATE-WORKER)
                            / WKS-PRINCIPAL * 100
             SIZE ERROR MOVE "J" TO COP-RETURN-CODE
                        MOVE 99.99 TO COP-POINTS.

       CTA-CALC-SERVICE-CHG.
           MOVE "C" TO COP-SERVICE-CHG-SW.
           COMPUTE COP-SERVICE-CHG ROUNDED =
                       COP-POINTS * WKS-PRINCIPAL * .01
             SIZE ERROR MOVE "J" TO COP-RETURN-CODE
                        MOVE WKS-ALL-NINES TO COP-SERVICE-CHG.
       CTA-EXIT.
           EXIT.


      * 35 OR 38
      *********************************************
       CTD-50-UNIT SECTION.
      *********************************************
           MOVE COP-TOTAL-NOTE TO COP-SERVICE-CHG.
           IF WKS-CTD-USE-NEW-MONEY
              SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM COP-SERVICE-CHG.

           IF SP-SCFRMLA = 38
              COMPUTE COP-SERVICE-CHG ROUNDED = COP-SERVICE-CHG
                         / 50 * SP-SCRATE
                SIZE ERROR MOVE "J" TO COP-RETURN-CODE
           ELSE
           IF SP-SCFRMLA = 35
              COMPUTE COP-SERVICE-CHG ROUNDED =
                              COP-SERVICE-CHG / 50
      * ALWAYS ROUND UP TO THE NEXT WHOLE DOLLAR  2.01 = 3.00  2.99 = 3.00
              MOVE COP-SERVICE-CHG TO WKS-AMOUNT-4
              IF WKS-AMOUNT-4-DEC > 0
                 ADD 1 TO WKS-AMOUNT-4-WHOLE
                 MOVE 0 TO WKS-AMOUNT-4-DEC
                 MOVE WKS-AMOUNT-4 TO COP-SERVICE-CHG
              END-IF
              COMPUTE COP-SERVICE-CHG ROUNDED =
                              COP-SERVICE-CHG * SP-SCRATE
                SIZE ERROR MOVE "J" TO COP-RETURN-CODE.


      *********************************************
       CTD-50-DOLLARS  SECTION.
      *********************************************
           MOVE COP-TOTAL-NOTE TO COP-SERVICE-CHG.
           IF WKS-CTD-USE-NEW-MONEY
              SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM COP-SERVICE-CHG.
           ADD 48.99 TO COP-SERVICE-CHG.
           DIVIDE 5000 INTO COP-SERVICE-CHG.
           COMPUTE COP-SERVICE-CHG ROUNDED = COP-SERVICE-CHG
                                          * 100 * SP-SCRATE
             SIZE ERROR MOVE "J" TO COP-RETURN-CODE.


      * SERVICE CHARGE FORMULA 3, 19, 30:
      *********************************************
       CTG-4-PERCENT  SECTION.
      *********************************************
           IF WKS-CTG-USE-NEW-MONEY AND COP-SOURCE-CD NOT = "SC"
              SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM COP-SERVICE-CHG.

           MULTIPLY .04 BY COP-SERVICE-CHG.

      * SERVICE CHARGE FORMULA 11:
      *********************************************
       CTG-PERCENT  SECTION.
      *********************************************
           MOVE COP-TOTAL-NOTE TO COP-SERVICE-CHG.
           IF WKS-CTG-USE-NEW-MONEY
              SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM COP-SERVICE-CHG.

           COMPUTE COP-SERVICE-CHG ROUNDED =
                       COP-SERVICE-CHG * SP-SCRATE * .01
                   SIZE ERROR MOVE "J" TO COP-RETURN-CODE.

      **************************
      *  4 16 27 52 59
      *********************************************
       CTJ-PERCENT SECTION.
      *********************************************
      **************************
           IF WKS-CTJ-SVCCHG-OK
              IF SP-SCFRMLA = 16
                 COMPUTE COP-SERVICE-CHG ROUNDED =
                      (COP-TOTAL-NOTE - COP-INT-CHARGE)
                         * .01 * SP-SCRATE
              ELSE
                 COMPUTE COP-SERVICE-CHG ROUNDED =
                         COP-AMT-FINANCD * .01 * SP-SCRATE
           ELSE
              MOVE 0 TO COP-SERVICE-CHG.


      *******************************
      * 23 43 44 45 50 57
      * 5 29 31 37 47 49 53
      * 55 = WORLD "IN"
      * 58 = WORLD "TX"
      *******************************
       CTM-FLAT-AMOUNT SECTION.
      *******************************
      * 58 ONLY 1 ALLOWED WITHIN 12 MONTHS (TXSCHG), WKS-CTJ-SVCCHG-OK
      * WILL TELL YOU IF AVAILABLE TO CHARGE OR RESTRICTED
           IF SP-SCFRMLA = 23 OR 29 OR 31 OR 37 OR 49 OR 53 OR 58
              IF WKS-CTJ-SVCCHG-OK
                 MOVE SP-SCRATE TO COP-SERVICE-CHG
              ELSE
                 MOVE 0 TO COP-SERVICE-CHG
           ELSE
              IF SP-SCFRMLA = 5
                 MOVE SP-SCRATE TO COP-SERVICE-CHG.

      * 55 NEEDS $150 AS FLAT RATE, SP-SCRATE ONLY HOLDS 99.99, SO USE
      *    SP-SCMAX
           IF SP-SCFRMLA = 55
              IF WKS-CTJ-SVCCHG-OK
                 MOVE SP-SCMAX TO COP-SERVICE-CHG
              ELSE
                 MOVE 0 TO COP-SERVICE-CHG.

      * 57 ONLY COMPUTE SERVICE CHARGE IF RENEWAL
      *    ONLY 2 ALLOWED WITHIN 12 MONTHS (NCSC23), WKS-CTJ-SVCCHG-OK
      *    WILL TELL YOU IF AVAILABLE TO CHARGE
           IF SP-SCFRMLA = 57
              IF NOT (COP-SOURCE-CD = "PB" OR "RN" OR "SC")
                 MOVE 0 TO COP-SERVICE-CHG
              ELSE
                 IF WKS-CTJ-SVCCHG-OK
                    MOVE SP-SCRATE TO COP-SERVICE-CHG
                 ELSE
                    MOVE 0 TO COP-SERVICE-CHG.

      * 43 "NC" FCF  NB-FB-SC-OT $24.00
      *              PB-RN       $15.00
      *     ONLY 2 WITHIN ANY 12 MONTH PERIOD
      * 44 "VA" FCF  NB-FB-SC-OT $24.00
      *              PB-RN       $15.00
      *     NO RESTRICTIONS

           IF SP-SCFRMLA = 43
              IF NOT WKS-CTJ-SVCCHG-OK
                 MOVE 0 TO COP-SERVICE-CHG.

           IF SP-SCFRMLA = 44 OR
                 (SP-SCFRMLA = 43 AND WKS-CTJ-SVCCHG-OK)
              IF COP-SOURCE-CD = "PB" OR "RN"
                 MOVE 15.00 TO COP-SERVICE-CHG
              ELSE
                 MOVE 24.00 TO COP-SERVICE-CHG.

      * 45 "NC" HCRED   0  - 2500  = 25.00
      *               2501 -  ?    = 1% OF CASH ADVANCE, NOT TO EXCEED 40.00
      *     ONLY 2 WITHIN ANY 12 MONTH PERIOD
           IF SP-SCFRMLA = 45
              IF NOT WKS-CTJ-SVCCHG-OK
                 MOVE 0 TO COP-SERVICE-CHG
              ELSE
                 COMPUTE WKS-PRINCIPAL = COP-TOTAL-NOTE - COP-INT-CHARGE
                                       - COP-SERVICE-CHG
                 IF WKS-PRINCIPAL NOT > 2500.00
                    MOVE 25.00 TO COP-SERVICE-CHG
                 ELSE
                    COMPUTE COP-SERVICE-CHG ROUNDED =
                                   WKS-PRINCIPAL * .01
                    IF COP-SERVICE-CHG > 40.00
                       MOVE 40.00 TO COP-SERVICE-CHG.

           IF SP-SCFRMLA = 50
              IF COP-SOURCE-CD = "NB" OR COP-RESTRICT-ADJ = 100.00
                 MOVE 100.00 TO COP-SERVICE-CHG
              ELSE
      * RESTRICT-ADJ WAS SET IN NCSC23 AND XONPC0
                 IF COP-RESTRICT-ADJ = 25.00
                    MOVE 25.00 TO COP-SERVICE-CHG
                 ELSE
                    MOVE 50.00 TO COP-SERVICE-CHG.

      *  BAY COUNTRY - THE RATE FIELD WAS NOT LARGE ENOUGH TO HOLD 125.00
      *              SO THEY WILL PUT 12.50 IN THERE AND WE MULTIPLY BY 10
      *       THAT WAY THEY CAN CHANGE AMOUNT DOWN THE ROAD
           IF SP-SCFRMLA = 47
              IF NOT (COP-SOURCE-CD = "PB" OR "RB" OR "RN" OR "SC")
                 MOVE 0 TO COP-SERVICE-CHG
              ELSE
                 IF WKS-CTJ-SVCCHG-OK
                    COMPUTE COP-SERVICE-CHG = SP-SCRATE * 10
                 ELSE
                    MOVE 0 TO COP-SERVICE-CHG.

      * NEBRASKA, GRAND ISLAND
      *********************************************
       CTM-SPLIT-24 SECTION.
      *********************************************
           COMPUTE WKS-PRINCIPAL =
               COP-TOTAL-NOTE - COP-INT-CHARGE - COP-SERVICE-CHG.
           IF WKS-CTG-USE-NEW-MONEY
              SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM WKS-PRINCIPAL.

           IF WKS-PRINCIPAL > 2000.00
              COMPUTE COP-SERVICE-CHG ROUNDED =
                 140.00 + (WKS-PRINCIPAL - 2000) * .05
           ELSE
              COMPUTE COP-SERVICE-CHG ROUNDED =
                 WKS-PRINCIPAL * .07.

      * REGENCY "OH"
      * SCFRMLA 39 = SECURED
      * SCFRMLA 40 = UNSECURED
      *********************************************
       CTM-SPLIT-RATE SECTION.
      *********************************************
           COMPUTE WKS-PRINCIPAL =
               COP-TOTAL-NOTE - COP-INT-CHARGE - COP-SERVICE-CHG.

           IF SP-SCFRMLA = 39
              EVALUATE TRUE
               WHEN WKS-PRINCIPAL <  500 MOVE 15.00 TO COP-SERVICE-CHG
               WHEN WKS-PRINCIPAL < 1000 MOVE 30.00 TO COP-SERVICE-CHG
               WHEN WKS-PRINCIPAL < 2000 MOVE 100.00 TO COP-SERVICE-CHG
               WHEN WKS-PRINCIPAL < 5000 MOVE 200.00 TO COP-SERVICE-CHG
               WHEN OTHER  COMPUTE COP-SERVICE-CHG ROUNDED =
                                  (WKS-PRINCIPAL * .01)
                               IF COP-SERVICE-CHG < 250.00
                                  MOVE 250.00 TO COP-SERVICE-CHG
                               END-IF
              END-EVALUATE.

           IF SP-SCFRMLA = 40
              EVALUATE TRUE
               WHEN WKS-PRINCIPAL <  500 MOVE 15.00 TO COP-SERVICE-CHG
               WHEN WKS-PRINCIPAL < 1000 MOVE 30.00 TO COP-SERVICE-CHG
               WHEN WKS-PRINCIPAL < 5000 MOVE 100.00 TO COP-SERVICE-CHG
               WHEN OTHER  COMPUTE COP-SERVICE-CHG ROUNDED =
                                  (WKS-PRINCIPAL * .01)
                               IF COP-SERVICE-CHG < 250.00
                                  MOVE 250.00 TO COP-SERVICE-CHG
                               END-IF
              END-EVALUATE.

      * IF REFINANCED WITHIN 90 DAYS, NO SERVICE CHARGE
           IF NOT WKS-CTJ-SVCCHG-OK
              MOVE 0 TO COP-SERVICE-CHG.

      * IF 39 RECALC INT CHGABLE TO INCLUDE CORRECT SVCCHG /0570
           IF SP-SCFRMLA = 39
              PERFORM AEA-CALC-INT-CHARGEABLE.

      **********************************************
      *  6 - WORLD TN
      * 56 - WORLD TX
      * 62 - WORLD TX WITH CLOSING FEE
      **********************************************
       CTP-FLAT-PERCENT  SECTION.
      **********************************************
           COMPUTE COP-SERVICE-CHG ROUNDED =
                       SP-SCRATE * COP-NEW-LOAN-PROCEEDS * .01
              SIZE ERROR MOVE "J" TO COP-RETURN-CODE
                         MOVE 99.99 TO COP-SERVICE-CHG.

      * 56 HAS A RESTRICTION IN NCSC23 FOR ONLY 1 CHARGE WITHIN 30 DAYS
      * USING A 360 YRTYPE, SO 30 DAYS IS A MONTH
      * WKS-CTJ-SVCCHG-SW IS TESTED FIRST, IN ZAA SECTION
           IF SP-SCFRMLA = 56
              IF NOT WKS-CTJ-SVCCHG-OK
                 MOVE 0 TO COP-SERVICE-CHG.

      * 62 HAS A CLOSING FEE OF 4% OF THE TILA AMOUNT FINANCED, UP TO
      * A MAX OF $50, ITEMZIED IN POINTS FIELD
           IF SP-SCFRMLA = 62
              COMPUTE WKS-CTU-EXTRA-4-PERCENT ROUNDED =
                        COP-APR-FINANCD * 0.04
              IF WKS-CTU-EXTRA-4-PERCENT < 0
                 MOVE 0 TO WKS-CTU-EXTRA-4-PERCENT
              END-IF
              IF WKS-CTU-EXTRA-4-PERCENT > 50
                 MOVE 50.00 TO WKS-CTU-EXTRA-4-PERCENT
              END-IF
              MOVE WKS-CTU-EXTRA-4-PERCENT TO COP-POINTS
              COMPUTE COP-SERVICE-CHG ROUNDED =
                      COP-SERVICE-CHG + WKS-CTU-EXTRA-4-PERCENT.

      *******************************************
       CTR-TOTREPAY SECTION.
      *******************************************
           COMPUTE COP-SERVICE-CHG ROUNDED =
                                COP-TOT-REPYBLE * .01 * SP-SCRATE
              SIZE ERROR MOVE "J" TO COP-RETURN-CODE.

      *******************************************
       CTR-TOTNOTE SECTION.
      *******************************************
           COMPUTE COP-SERVICE-CHG ROUNDED =
                              COP-TOTAL-NOTE * .01 * SP-SCRATE
              SIZE ERROR MOVE "J" TO COP-RETURN-CODE.

      *******************************************
      * SCFRMLA = 7 OR 25 OR 26
      *******************************************
       CTS-NEW-MONEY-PERCENT SECTION.
      *******************************************
           MOVE COP-AMT-FINANCD TO COP-SERVICE-CHG.
      * = "0"
           IF WKS-CTS-SVCCHG-OK
              SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM COP-SERVICE-CHG.
           COMPUTE COP-SERVICE-CHG ROUNDED =
                            COP-SERVICE-CHG * .01 * SP-SCRATE
              SIZE ERROR MOVE "J" TO COP-RETURN-CODE.


      * 13 - NEW SOUTH (LENDMARK) "GA", SAME AS 12 EXCEPT A "SC" LOAN
      *      IS TREATED JUST AS A "NB", NO NEW MONIES ONLY RESTRICTION
      * 54 - SAME AS 12-13 BUT ADDS AN EXTRA 4% AFTER THE REST IS CALCED

      *********************************************
       CTU-GEORGIA-12-13-54  SECTION.
      *********************************************
      * SET THE AMOUNT TO CHARGE SC ON:
           IF COP-TERM > 18
              COMPUTE WKS-CTU-8-PERCENT = COP-TOTAL-NOTE
                          - COP-INT-CHARGE - COP-SERVICE-CHG
           ELSE
              MOVE COP-TOTAL-NOTE TO WKS-CTU-8-PERCENT.

      * BREAKOUT FOR 8% AND 4% SERVICE CHARGEABLE:
           IF WKS-CTU-8-PERCENT > 600
              SUBTRACT 600 FROM WKS-CTU-8-PERCENT
                                     GIVING WKS-CTU-4-PERCENT
              MOVE 600 TO WKS-CTU-8-PERCENT
           ELSE
              MOVE 0 TO WKS-CTU-4-PERCENT.

      * REDUCE SERVICE CHARGEABLE BY DUAL LOAN MONIES :
           IF COP-DUAL-LOAN-ADJ NOT = 0
              IF COP-DUAL-LOAN-ADJ > 600
                 ADD WKS-CTU-8-PERCENT TO WKS-CTU-4-PERCENT
                 MOVE 0 TO WKS-CTU-8-PERCENT
              ELSE
                 COMPUTE WKS-CTU-4-PERCENT =
                           WKS-CTU-4-PERCENT + WKS-CTU-8-PERCENT
                                    - (600 - COP-DUAL-LOAN-ADJ)
                 IF WKS-CTU-4-PERCENT > 0
                    COMPUTE WKS-CTU-8-PERCENT =
                           (600 - COP-DUAL-LOAN-ADJ)
                 ELSE
                    MOVE 0 TO WKS-CTU-4-PERCENT.

      * REDUCE SERVICE CHARGEABLE BY RESTRICTED MONIES
      * FROM RENEWALS AND 15 DAY PAID OUTS:
      * 13 WILL NOT HAVE WKS-CTU-USE-NEW-MONEY SET ON !!
           MOVE COP-RESTRICT-ADJ TO WKS-CTU-PRIOR-MONEY.
           IF WKS-CTU-USE-NEW-MONEY
              ADD WKS-TOTPRIOR-LOAN-BALANCE TO WKS-CTU-PRIOR-MONEY.

           IF WKS-CTU-PRIOR-MONEY > WKS-CTU-8-PERCENT
              COMPUTE WKS-CTU-4-PERCENT = WKS-CTU-4-PERCENT
                 - (WKS-CTU-PRIOR-MONEY - WKS-CTU-8-PERCENT)
              MOVE 0 TO WKS-CTU-8-PERCENT
           ELSE
              SUBTRACT WKS-CTU-PRIOR-MONEY FROM WKS-CTU-8-PERCENT.


      * COMPUTE SERVICE CHARGES:
       CTU-GEORGIA-12-13-54-CALC.
           IF WKS-CTU-8-PERCENT < 0
              MOVE 0 TO WKS-CTU-8-PERCENT.
           COMPUTE COP-SERVICE-CHG ROUNDED
                                   COP-STATE-WORKER ROUNDED =
                                           WKS-CTU-8-PERCENT * .08
               SIZE ERROR MOVE "J" TO COP-RETURN-CODE.

           IF WKS-CTU-4-PERCENT < 0
              MOVE 0 TO WKS-CTU-4-PERCENT.
           COMPUTE COP-SERVICE-CHG ROUNDED =
                        COP-SERVICE-CHG + WKS-CTU-4-PERCENT * .04
               SIZE ERROR MOVE "J" TO COP-RETURN-CODE.

           IF SP-SCFRMLA = 54
              COMPUTE WKS-CTU-EXTRA-4-PERCENT ROUNDED =
                        COP-TOTAL-NOTE * 0.04
              IF WKS-CTU-EXTRA-4-PERCENT < 0
                 MOVE 0 TO WKS-CTU-EXTRA-4-PERCENT
              END-IF
              IF WKS-CTU-EXTRA-4-PERCENT > 50
                 MOVE 50.00 TO WKS-CTU-EXTRA-4-PERCENT
              END-IF
              MOVE WKS-CTU-EXTRA-4-PERCENT TO COP-POINTS
              COMPUTE COP-SERVICE-CHG ROUNDED =
                      COP-SERVICE-CHG + WKS-CTU-EXTRA-4-PERCENT.
       CTU-EXIT.
           EXIT.

      *********************************************
       CTW-MICHIGAN-14-21-22  SECTION.
      *********************************************

           IF SP-SCFRMLA = 14 OR 21
              IF COP-SOURCE-CD = "PB" OR "RN" OR "RB" OR "RO"
                 MOVE 0 TO COP-SERVICE-CHG
                 GO TO CTW-EXIT.

           SUBTRACT COP-INT-CHARGE FROM COP-TOTAL-NOTE
                                             GIVING WKS-PRINCIPAL.
           IF SP-SCFRMLA = 21
              SUBTRACT COP-SERVICE-CHG FROM WKS-PRINCIPAL.

      * SCFRMLA 22 NEEDS TO ROUND DOWN
           IF SP-SCFRMLA = 22
              COMPUTE COP-SERVICE-CHG =
                                WKS-PRINCIPAL * .01 * SP-SCRATE
                  SIZE ERROR MOVE "J" TO COP-RETURN-CODE
           ELSE
              COMPUTE COP-SERVICE-CHG ROUNDED =
                                WKS-PRINCIPAL * .01 * SP-SCRATE
                  SIZE ERROR MOVE "J" TO COP-RETURN-CODE.

       CTW-EXIT.
           EXIT.

      **************************************************
       CTX-TENNESSEE-15-17-60-61 SECTION.
      **************************************************

      * SHALL NEVER BE IMPOSED ON ANY PORTION OF ANY LOAN USED TO PAY
      * ANY EXISTING LOAN OWING BY THE SAME BORROWER
           SUBTRACT WKS-PRIOR-LOAN-BALANCE FROM COP-TOTAL-NOTE
                                            GIVING WKS-PRINCIPAL.

      * ONLY DIFFERENCE BETWEEN 17 AND 60/61 IS THE RATE
           IF SP-SCFRMLA = 17
              MOVE 0.04 TO WS-RATE
           ELSE
              MOVE 0.05 TO WS-RATE.

           MOVE 2.00 TO COP-SERVICE-CHG.
      * FLAT $2.00 ON ALL LOANS WHICH DO NOT EXCEED 20.00
           IF WKS-PRINCIPAL > 20.00
      * AND .50C PER EACH $5.00 INCREASE IN EXCESS OF $20.00
      * NOT TO EXCEED $100.00
              IF WKS-PRINCIPAL NOT > 100.00
                 COMPUTE WKS-GO-TO = (WKS-PRINCIPAL - 20) / 5
                 COMPUTE COP-SERVICE-CHG = COP-SERVICE-CHG +
                                           (WKS-GO-TO * .50)
              ELSE
      * LOANS OVER $100
                 IF SP-SCFRMLA = 17 OR 60 OR 61
                    COMPUTE COP-SERVICE-CHG ROUNDED =
                                      WKS-PRINCIPAL * WS-RATE
                    IF COP-SERVICE-CHG NOT = 0
                               AND COP-SERVICE-CHG < 10.00
                       MOVE 10.00 TO COP-SERVICE-CHG
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    MOVE 10.00 TO COP-SERVICE-CHG.

      * USE-HALF FLAG IF REFINANCED OR RENEWED WITHIN 45 DAYS
      * CLOSING FEE IS NOT TO BE INCLUDED IN THIS
           IF WKS-CTG-USE-HALF
              COMPUTE COP-SERVICE-CHG ROUNDED = COP-SERVICE-CHG / 2.

      * SERVICE CHARGE FORMULA 60 WILL INCLUDE A CLOSING FEE OF 4% OF THE
      * TILA AMOUNT FINANCED, UP TO A MAXIMUM OF $50. ITEMIZE IN POINTS FIELD
           IF SP-SCFRMLA = 60
              COMPUTE WKS-CTU-EXTRA-4-PERCENT ROUNDED =
                        COP-APR-FINANCD * 0.04
              IF WKS-CTU-EXTRA-4-PERCENT < 0
                 MOVE 0 TO WKS-CTU-EXTRA-4-PERCENT
              END-IF
              IF WKS-CTU-EXTRA-4-PERCENT > 50
                 MOVE 50.00 TO WKS-CTU-EXTRA-4-PERCENT
              END-IF
              MOVE WKS-CTU-EXTRA-4-PERCENT TO COP-POINTS
              COMPUTE COP-SERVICE-CHG ROUNDED =
                      COP-SERVICE-CHG + WKS-CTU-EXTRA-4-PERCENT.

       CTX-EXIT.
           EXIT.

      *********************************************
       CTY-SCAROLINA-18-42 SECTION.
      *********************************************

      * COMPUTE NEW CASH ADVANCE ON THIS LOAN:
           COMPUTE WKS-PRINCIPAL =
                       COP-TOTAL-NOTE
                         - COP-INT-CHARGE
                           - COP-SERVICE-CHG.

      * COMPUTE SERVICE CHARGE NO RESTRICTIONS:
           COMPUTE COP-SERVICE-CHG ROUNDED =
                          WKS-PRINCIPAL * (SP-SCRATE * 0.01).

      * JTG 022796
      * CHANGED TO ALLOW MAXIMUM SERVICE CHARGE, ON OTHER THAN RENEWALS:
           IF NOT (COP-SOURCE-CD = "PB" OR "RN" OR "RB" OR "RO")
              GO TO CTY-SCAROLINA-18-42-EXIT.

      * ESTABLISH THE HIGHEST CASH ADVANCE MADE, INCLUDING
      * THIS LOAN:
           IF WKS-PRINCIPAL < COP-RESTRICT-HIGH-CASHADV
              MOVE COP-RESTRICT-HIGH-CASHADV TO WKS-PRINCIPAL.

      * DETERMINE THE MAXIMUM CASH ADVANCE ALLOWED:
           COMPUTE WKS-AMOUNT ROUNDED =
                    SP-SCMAX / (SP-SCRATE * 0.01).

      * CAP HIGHEST CASH ADVANCE MADE TO MAXIMUM ALLOWED:
           IF WKS-PRINCIPAL > WKS-AMOUNT
              MOVE WKS-AMOUNT TO WKS-PRINCIPAL.

      * DETERMINE THE CASH ADVANCE USED IN LAST 3 MONTHS:
           COMPUTE WKS-AMOUNT-2 ROUNDED =
                    COP-RESTRICT-SERCHG / (SP-SCRATE * 0.01).

      * DETERMINE EXCESS CASH ADVANCE THAT SERVICE CHARGE CAN
      * BE CHARGED ON:
           SUBTRACT WKS-AMOUNT-2 FROM WKS-PRINCIPAL.

      * COMPUTE SERVICE CHARGE RE: RESTRICTIONS:
           IF WKS-PRINCIPAL > 0
              COMPUTE WKS-AMOUNT-2 =
                            WKS-PRINCIPAL * (SP-SCRATE * 0.01)
              IF WKS-AMOUNT-2 < COP-SERVICE-CHG
                 MOVE WKS-AMOUNT-2 TO COP-SERVICE-CHG
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE 0 TO COP-SERVICE-CHG.


       CTY-SCAROLINA-18-42-EXIT.
           EXIT.


      *********************************************
       CXF-GET-INS-FINCHG SECTION.
      *********************************************
           MOVE 0 TO WKS-INS-FINCHG.
           IF COP-INS-IS-FINCHG(1)
              ADD COP-INS-PREMIUM(1) TO WKS-INS-FINCHG.
           IF COP-INS-IS-FINCHG(2)
              ADD COP-INS-PREMIUM(2) TO WKS-INS-FINCHG.
           IF COP-INS-IS-FINCHG(3)
              ADD COP-INS-PREMIUM(3) TO WKS-INS-FINCHG.
           IF COP-INS-IS-FINCHG(4)
              ADD COP-INS-PREMIUM(4) TO WKS-INS-FINCHG.
           IF COP-INS-IS-FINCHG(5)
              ADD COP-INS-PREMIUM(5) TO WKS-INS-FINCHG.
           IF COP-INS-IS-FINCHG(6)
              ADD COP-INS-PREMIUM(6) TO WKS-INS-FINCHG.


      *********************************************
       DZA-CLEANUP-UP-FINAL  SECTION.
      *********************************************
           PERFORM GET-ALL-INS-PREMIUMS.
           SUBTRACT COP-NEW-LOAN-PROCEEDS COP-SERVICE-CHG
                    WKS-ALL-INS-PREMIUMS
                    WKS-ALL-INS-TAX
                           FROM COP-TOTAL-NOTE GIVING COP-INT-CHARGE.

           IF COP-INTEREST-BEARING
              MOVE COP-SERVICE-CHG TO COP-ANTIC-EARNS
              COMPUTE COP-LOAN-AMOUNT = COP-TOT-REPYBLE
                         - COP-INT-CHARGE - COP-1ST-PY-EXT
      * THIS CODE WAS IN HERE 4.30.91  I CHANGED BACK TO F45 BECAUSE
      * WE DONT KNOW WHY EXT CHARGE WAS BEING IGNORED
      *       SUBTRACT COP-INT-CHARGE FROM COP-TOT-REPYBLE
      *                                 GIVING COP-LOAN-AMOUNT
           ELSE
              ADD COP-INT-CHARGE COP-1ST-PY-EXT COP-SERVICE-CHG
                                 COP-MAINT-FEES GIVING COP-ANTIC-EARNS
              MOVE COP-TOT-REPYBLE TO COP-LOAN-AMOUNT.

       DZA-EXIT.
           EXIT.


      *********************************************
       EAA-FINAL-CALCULATIONS SECTION.
      *********************************************

      * THIS FLAG IS SET TO 'Y' TO SET TESTING OF CALCULATED
      * SERVICE CHARGE FROM INPUTED POINTS.
      
           IF COP-SERVICE-CHG-IS-CALCD
              IF POINTS-TEST-MAX-FG = "N"
                 IF (COP-SERVICE-CHG > SP-SCMAX) OR
                    (COP-SERVICE-CHG < SP-SCMIN)
                    MOVE "Y" TO POINTS-TEST-MAX-FG
                    MOVE "%" TO COP-RETURN-CODE
                    GO TO EAA-EXIT.

      *-----------------------------------------------------------------
      *    THE FOLLOWING LOGIC WAS ADDED TO CORRECT THRASHING IN
      *    AAA-LOOP ROUTINE, CAUSING EXIT DUE TO COMPARES
      *    BEING WITH IN TOLERANCE LEVELS.
      *-----------------------------------------------------------------
           PERFORM GET-ALL-INS-PREMIUMS.
           COMPUTE COP-TOT-REPYBLE = COP-REG-PAYMENT * COP-TERM
                                                  + COP-1ST-PY-EXT
              SIZE ERROR MOVE WKS-ALL-NINES TO COP-TOT-REPYBLE
                         MOVE "D" TO COP-RETURN-CODE
                         GO TO EAA-EXIT.
           IF COP-INTEREST-BEARING
              COMPUTE COP-LOAN-AMOUNT = COP-TOT-REPYBLE
                         - COP-INT-CHARGE - COP-1ST-PY-EXT
           ELSE
              MOVE COP-TOT-REPYBLE TO COP-LOAN-AMOUNT.

           ADD COP-PROCEEDS
               COP-GL-AMOUNT-TOTAL
               WKS-PRIOR-LOAN-BALANCE
               WKS-ALL-INS-PREMIUMS
               WKS-ALL-INS-TAX
                          GIVING COP-AMT-FINANCD.

      *-----------------------------------------------------------------
      *    GET AMOUNT FINANCED FOR APR
      *-----------------------------------------------------------------
           PERFORM CXF-GET-INS-FINCHG.
           SUBTRACT WKS-INS-FINCHG
                      COP-GL-AMOUNT-TOTAL-FINCHG
              FROM COP-AMT-FINANCD GIVING COP-APR-FINANCD.

      *-----------------------------------------------------------------
      *      COMPUTE FINANCE CHARGE
      *-----------------------------------------------------------------
           COMPUTE COP-FINCHG = COP-INT-CHARGE
                    + COP-1ST-PY-EXT + COP-SERVICE-CHG
                    + COP-MAINT-FEES.
           ADD WKS-INS-FINCHG TO COP-FINCHG.
           ADD COP-GL-AMOUNT-TOTAL-FINCHG TO COP-FINCHG.

           IF COP-FINCHG < SP-MINSF-CHG
              MOVE "M" TO COP-RETURN-CODE.

      *-----------------------------------------------------------------
      *  HERE COMES THE APR SETUP AND CALCULATION.
      *  NOTE:
      *       WKS-SPAY-FG (APR-SPAY-FG) IS CLEARED HERE TO YIELD
      *       CORRECT APR (365/DAYS ALTERNATIVE SINGLE PAY CALC)
      *       DURING MAIN LOOP CALCS FLAG HAD TO BE SET AS TO NOT
      *       INTERFERE WITH CALC OF INTEREST.
      *-----------------------------------------------------------------
           MOVE " " TO WKS-SPAY-FG.

           PERFORM XAA-CALCULATE-APR.

           ADD 0 APR-APR GIVING WKS-ROUNDV999 ROUNDED.
           MOVE WKS-ROUNDV999 TO COP-APR-RATE CMP-APR-RATE.

      *-----------------------------------------------------------------
      *  AND HERE THE SIMPLE INTEREST CALCULATIONS.
      *-----------------------------------------------------------------
      *    IF SP-PYEXFRMLA-REG-INCR
      *       PERFORM XBA-CALCULATE-SIMPLE-INTEREST
      *    ELSE
      *       MOVE SP-DISFRMLA TO WKS-DISFRMLA
      *       MOVE "B" TO SP-DISFRMLA
      *       PERFORM XBA-CALCULATE-SIMPLE-INTEREST
      *       MOVE WKS-DISFRMLA TO SP-DISFRMLA.
      *    ADD 0 APR-APR GIVING WKS-ROUNDV999 ROUNDED.
      *    MOVE WKS-ROUNDV999 TO COP-SIM-RATE CMP-SIM-RATE.

           IF SP-PYEXFRMLA-REG-INCR
              PERFORM APRS-GET-SIMPLE-RATE
           ELSE
              MOVE SP-DISFRMLA TO WKS-DISFRMLA
              MOVE "B" TO SP-DISFRMLA
              PERFORM APRS-GET-SIMPLE-RATE
              MOVE WKS-DISFRMLA TO SP-DISFRMLA.

           ADD 0 APR-APR GIVING WKS-ROUNDV999 ROUNDED.
           MOVE WKS-ROUNDV999 TO COP-SIM-RATE CMP-SIM-RATE.

      *-----------------------------------------------------------------
      *  AND HERE THE EFFECTIVE INTEREST CALCULATIONS.
      *-----------------------------------------------------------------
           IF SP-CAL-SIMPLE(CTBL) OR SP-CAL-FLAT-MONTHLY(CTBL)
              MOVE WKS-ROUNDV999 TO COP-EFF-RATE CMP-EFF-RATE
              GO TO EAA-RESET-MEMOED-INS.

       EAA-CALC-ADDON.
           IF NOT SP-CAL-ADDON(CTBL)
              GO TO EAA-CALC-DISCOUNT.

      *    IF SP-PYEXFRMLA-REG-INCR
      *       AND COP-1ST-PMT-EXTENSION NOT = 0
      *      PERFORM XCA-ADDON-WITH-EXTENSION
      *      GO TO EAA-RESET-MEMOED-INS.

           IF SP-PYEXFRMLA-REG-INCR
              COMPUTE WKS-ROUNDV999 ROUNDED =
                  COP-INT-CHARGE * 1200
                    / (COP-INT-CHARGEABLE
                         * (COP-TERM + INT-MTHS + INT-DAYS/30))
      *               + 0.0009999
           ELSE
              COMPUTE WKS-ROUNDV999 ROUNDED = COP-INT-CHARGE
                * 1200 / (COP-INT-CHARGEABLE * COP-TERM).
      *         * 1200 / (COP-INT-CHARGEABLE * COP-TERM) + .0009999.
           MOVE WKS-ROUNDV999 TO COP-EFF-RATE CMP-EFF-RATE.
           GO TO EAA-RESET-MEMOED-INS.

       EAA-CALC-DISCOUNT.
      *    IF SP-PYEXFRMLA-REG-INCR
      *      AND COP-1ST-PMT-EXTENSION NOT = 0
      *      PERFORM XDA-DISCOUNT-WITH-EXTENSION
      *      GO TO EAA-RESET-MEMOED-INS.

           IF SP-PYEXFRMLA-REG-INCR
              COMPUTE WKS-ROUNDV999 ROUNDED =
                  1200 / (COP-TERM + INT-MTHS + INT-DAYS/30)
                   * (1 - 1 / (1 + COP-INT-CHARGE
                                           / COP-INT-CHARGEABLE)
                     )
      *              + 0.0009999
           ELSE
              COMPUTE WKS-ROUNDV999 ROUNDED = 1200 / COP-TERM
                * (1 - 1 / (1 + COP-INT-CHARGE / COP-INT-CHARGEABLE)).
      *                                                   + .0009999.
           MOVE WKS-ROUNDV999 TO COP-EFF-RATE CMP-EFF-RATE.

       EAA-RESET-MEMOED-INS.
           IF COP-INS-USE-IS-MEMO(1)
             MOVE CIP-INS-PREMIUM(1) TO CMP-INS-PREMIUM(1)
                                        COP-INS-PREMIUM(1).
           IF COP-INS-USE-IS-MEMO(2)
             MOVE CIP-INS-PREMIUM(2) TO CMP-INS-PREMIUM(2)
                                        COP-INS-PREMIUM(2).
           IF COP-INS-USE-IS-MEMO(3)
             MOVE CIP-INS-PREMIUM(3) TO CMP-INS-PREMIUM(3)
                                        COP-INS-PREMIUM(3).
           IF COP-INS-USE-IS-MEMO(4)
             MOVE CIP-INS-PREMIUM(4) TO CMP-INS-PREMIUM(4)
                                        COP-INS-PREMIUM(4).
           IF COP-INS-USE-IS-MEMO(5)
             MOVE CIP-INS-PREMIUM(5) TO CMP-INS-PREMIUM(5)
                                        COP-INS-PREMIUM(5).
           IF COP-INS-USE-IS-MEMO(6)
             MOVE CIP-INS-PREMIUM(6) TO CMP-INS-PREMIUM(6)
                                        COP-INS-PREMIUM(6).
           IF COP-INS-USE-IS-MEMO(7)
             MOVE CIP-INS-PREMIUM(7) TO CMP-INS-PREMIUM(7)
                                        COP-INS-PREMIUM(7).
           IF COP-INS-USE-IS-MEMO(8)
             MOVE CIP-INS-PREMIUM(8) TO CMP-INS-PREMIUM(8)
                                        COP-INS-PREMIUM(8).

       EAA-FILL-IN-PC0-EDITS.
           IF CIP-NO-OF-TIMES-CALLED > 0 AND < 800
              GO TO EAA-EXIT.
           MOVE 0 TO CEP-EFF-RATE CEP-APR-RATE.
           PERFORM EBA-SET-RATE-LIMITS.
           IF CEP-EFF-RATE = 0
              MOVE 9999.9999 TO CEP-EFF-RATE.
           IF CEP-APR-RATE = 0
              MOVE 9999.9999 TO CEP-APR-RATE.

           MOVE COP-INS-TERM(1) TO CEP-INS-TERM(1).
           MOVE COP-INS-TERM(2) TO CEP-INS-TERM(2).
           MOVE COP-INS-TERM(3) TO CEP-INS-TERM(3).
           MOVE COP-INS-TERM(4) TO CEP-INS-TERM(4).
           MOVE COP-INS-TERM(5) TO CEP-INS-TERM(5).
           MOVE COP-INS-TERM(6) TO CEP-INS-TERM(6).
           MOVE COP-INS-TERM(7) TO CEP-INS-TERM(7).
           MOVE COP-INS-TERM(8) TO CEP-INS-TERM(8).

           MOVE COP-INS-PREMIUM(1) TO CEP-INS-PREMIUM(1).
           MOVE COP-INS-PREMIUM(2) TO CEP-INS-PREMIUM(2).
           MOVE COP-INS-PREMIUM(3) TO CEP-INS-PREMIUM(3).
           MOVE COP-INS-PREMIUM(4) TO CEP-INS-PREMIUM(4).
           MOVE COP-INS-PREMIUM(5) TO CEP-INS-PREMIUM(5).
           MOVE COP-INS-PREMIUM(6) TO CEP-INS-PREMIUM(6).
           MOVE COP-INS-PREMIUM(7) TO CEP-INS-PREMIUM(7).
           MOVE COP-INS-PREMIUM(8) TO CEP-INS-PREMIUM(8).

           MOVE COP-INS-COVERAGE(1) TO CEP-INS-COVERAGE(1).
           MOVE COP-INS-COVERAGE(2) TO CEP-INS-COVERAGE(2).
           MOVE COP-INS-COVERAGE(3) TO CEP-INS-COVERAGE(3).

      * BOTH OF THE FOLLOWING 2 IF'S CAN BE TRUE  5 IS VALID FOR BOTH
      *  DO   NOT  PUT   AN   'ELSE'  ON  THEM !!
           IF SP-ADJ-INSCOV-BY-O2(6)
              ADD COP-INS-COVERAGE(5) TO CEP-INS-COVERAGE(3).
           IF SP-ADJ-INSCOV-BY-O3(6)
              ADD COP-INS-COVERAGE(6) TO CEP-INS-COVERAGE(3).
           IF COP-INS-PP
              IF SP-MAX-OVERRIDE(6)
                 MOVE 999999.99     TO CEP-INS-COVERAGE(3)
                                       CEP-INS-PREMIUM(3).
           IF COP-INS-PP2
              IF SP-MAX-OVERRIDE(7)
                 MOVE 999999.99     TO CEP-INS-COVERAGE(3)
                                       CEP-INS-PREMIUM(3).
           MOVE COP-INS-COVERAGE(4) TO CEP-INS-COVERAGE(4).
           MOVE COP-INS-COVERAGE(5) TO CEP-INS-COVERAGE(5).
           MOVE COP-INS-COVERAGE(6) TO CEP-INS-COVERAGE(6).
           MOVE COP-INS-COVERAGE(7) TO CEP-INS-COVERAGE(7).
           MOVE COP-INS-COVERAGE(8) TO CEP-INS-COVERAGE(8).
           MOVE COP-SERVICE-CHG     TO CEP-SERVICE-CHG.
           MOVE COP-MAINT-FEES      TO CEP-MAINT-FEES.
           MOVE COP-1ST-PY-EXT      TO CEP-1ST-PY-EXT.
           MOVE COP-REG-PAYMENT     TO CEP-REG-PAYMENT.

       EAA-EXIT.
           EXIT.

      **************************************************
      *    COMPUTE THE SIMPLE INTEREST RATE
      *    AS FINAL COMPUTATION TO MATCH
      *    LIBLP/LPAPRS CALLED IN UPDATE PROGRAMS
      *    LP/LONPB2.C & LP/LONPC2.C
      **************************************************
       APRS-GET-SIMPLE-RATE SECTION.
      * COMPUTE TRUE INTEREST CHARGABLE:
           MOVE 0 TO APRS-PYAMT-ADJ.
           IF SP-CAL-CHGABLE(CTBL) = "P" OR "S"
              PERFORM GET-ALL-INS-PREMIUMS
              SUBTRACT WKS-ALL-INS-PREMIUMS WKS-ALL-INS-TAX
                   FROM APRS-PYAMT-ADJ.
           IF SP-CAL-CHGABLE(CTBL) = "P" OR "I"
              SUBTRACT COP-SERVICE-CHG FROM APRS-PYAMT-ADJ.

           COMPUTE APR-FINANCED =
                COP-LOAN-AMOUNT - COP-MAINT-FEES + APRS-PYAMT-ADJ.

           IF COP-LOANTYPE = "P"
              SUBTRACT COP-INT-CHARGE COP-1ST-PY-EXT
                                   FROM APR-FINANCED.

      * DERIVE PAYMENT AMOUNTS FROM TRUE INTEREST CHARGABLE:
           COMPUTE APRS-PYAMT-ADJ =
                   APRS-PYAMT-ADJ / COP-TERM.

           IF COP-ODD-1ST-PMT = 0
              MOVE COP-TERM        TO APR-NOREGPY
              MOVE 0               TO APR-NOODDPY
                                      APR-1STPYAMT
           ELSE
              SUBTRACT 1 FROM COP-TERM GIVING APR-NOREGPY
              MOVE 1               TO APR-NOODDPY
              MOVE COP-ODD-1ST-PMT TO APR-1STPYAMT
              SUBTRACT COP-1ST-PY-EXT WKS-MTHLY-MAINT-FEES
                                 FROM APR-1STPYAMT
              ADD APRS-PYAMT-ADJ   TO APR-1STPYAMT.

           COMPUTE APR-REGPYAMT =
                   COP-REG-PAYMENT - WKS-MTHLY-MAINT-FEES
                                   + APRS-PYAMT-ADJ.

      * ----------------------------------------------------------------------
           MOVE COP-IN-DT        TO APR-INTDATE.
           MOVE COP-PMT-DT       TO APR-1STPYDATE.
           MOVE WKS-SPAY-FG      TO APR-SPAY-FG.

           PERFORM APR-REGZ.

       APRS-GET-EFFRATE-EXIT.
           EXIT.

      ******************************************
       EBA-SET-RATE-LIMITS  SECTION.
      ******************************************
           SET ETBL TO CTBL.
           SET COP-ETBL CMP-ETBL TO ETBL.
           PERFORM VARYING ESTP FROM 1 BY 1
                       UNTIL ESTP > SP-EDT-NOEDITS(ETBL)
                                  OR NOT COP-UNKNOWN-ERROR
               IF SP-EDT-TYPE(ETBL ESTP) = "R"
                           AND SP-EDT-HL(ETBL ESTP) = "H"
                  IF SP-EDT-RATETYPE(ETBL ESTP) = "A"
                     IF SP-EDT-RATE(ETBL ESTP) > CEP-APR-RATE
                        MOVE SP-EDT-RATE(ETBL ESTP) TO CEP-APR-RATE
                     END-IF
                  ELSE
                     IF SP-EDT-RATE(ETBL ESTP) > CEP-EFF-RATE
                        MOVE SP-EDT-RATE(ETBL ESTP) TO CEP-EFF-RATE
                     END-IF
                  END-IF
               END-IF
           END-PERFORM.

       EBA-EXIT.
           EXIT.

      **************************************
       EDA-CHECK-MAX-RATES  SECTION.
      **************************************
           SET ETBL TO CTBL.
           SET COP-ETBL CMP-ETBL TO ETBL.
           PERFORM EDA-HANDLE-EACH-APR
             VARYING ESTP FROM 1 BY 1
               UNTIL ESTP > SP-EDT-NOEDITS(ETBL)
                  OR NOT COP-UNKNOWN-ERROR.
           GO TO EDA-EXIT.

       EDA-HANDLE-EACH-APR.
           IF SP-EDT-TYPE(ETBL,ESTP) = "R"
              IF SP-EDT-RATETYPE(ETBL,ESTP) = "A"
                 IF (SP-EDT-HL(ETBL,ESTP) = "H") AND
                    (COP-APR-RATE > SP-EDT-RATE(ETBL,ESTP))
      *
      *             MOVE SP-EDT-RATE(ETBL,ESTP) TO COP-OVR-RATE
      *                  CEP-APR-RATE
                    MOVE "O" TO COP-RETURN-CODE
                    MOVE "APR RATE EXCEEDS MAXIMUM IN SPR" TO MESS
                    PERFORM SEND-MESS
                    MOVE "A" TO COP-OVR-RATE-SW
                 ELSE
                    IF (SP-EDT-HL(ETBL,ESTP) = "L") AND
                       (COP-APR-RATE < SP-EDT-RATE(ETBL,ESTP))
      *                MOVE SP-EDT-RATE(ETBL,ESTP) TO COP-OVR-RATE
                       MOVE "O" TO COP-RETURN-CODE
                       MOVE "APR RATE BELOW MINIMUM IN SPR" TO MESS
                       PERFORM SEND-MESS
                       MOVE "A" TO COP-OVR-RATE-SW
                    ELSE
                       NEXT SENTENCE
              ELSE
                 IF (SP-EDT-HL(ETBL,ESTP) = "H") AND
                    (COP-EFF-RATE > SP-EDT-RATE(ETBL,ESTP))
                    MOVE SP-EDT-RATE(ETBL,ESTP) TO COP-OVR-RATE
                         CEP-EFF-RATE
                    MOVE "O" TO COP-RETURN-CODE
                    MOVE "E" TO COP-OVR-RATE-SW
                 ELSE
                    IF (SP-EDT-HL(ETBL,ESTP) = "L") AND
                       (COP-EFF-RATE < SP-EDT-RATE(ETBL,ESTP))
                       MOVE SP-EDT-RATE(ETBL,ESTP) TO COP-OVR-RATE
                       MOVE "O" TO COP-RETURN-CODE
                       MOVE "E" TO COP-OVR-RATE-SW
                    ELSE
                       NEXT SENTENCE.

       EDA-EXIT.
           EXIT.


      ***************************************************************
      *    PRE CALCULATION EDITS
      ***************************************************************
       MAA-INPUT-EDIT  SECTION.
      *--------------------------------------------------------------
      *    FOR WORLD CAN LOANS:
      *    ADDED LOGIC TO FORCE ONE CALENDAR MONTH TO FIRST PAYMENT
      *    WHILE IN THIS CALCULATOR, LONPC2.C WILL RECOMPUTE THE APR
      *    AND EFFECTIVE RATE USING TRUE FIRST PAYMENT DATE
      *--------------------------------------------------------------
           IF CIP-CAN-LOAN-NO NOT = " "
              MOVE CIP-LN-DT TO SAVE-CIP-LOANDATE
              MOVE CIP-IN-DT TO NUM-DATE
                                SAVE-CIP-INTDATE
              MOVE CIP-PMT-DT TO SYS-DATE
                                 SAVE-CIP-1STPYDATE
      *       FORCE 1ST PAYMENT TO ONE MONTH FROM INTEREST DATE:
              IF NUM-DA NOT = S-DD
                 MOVE CIP-IN-DT TO NDTE-DATE
                 MOVE 5 TO NDTE-DD
                 MOVE NDTE-DATE TO CIP-IN-DT
                 MOVE 1 TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO CIP-PMT-DT.
       MAA-EXIT.
           EXIT.

      ***************************************************************
      *    RESTORE CALCULATION EDITS
      ***************************************************************
       MAB-INPUT-EDIT-RESET  SECTION.
           IF CIP-CAN-LOAN-NO NOT = " "
              MOVE SAVE-CIP-LOANDATE TO CIP-LN-DT
                                        COP-LN-DT
              MOVE SAVE-CIP-INTDATE TO CIP-IN-DT
                                       COP-IN-DT
              MOVE SAVE-CIP-1STPYDATE TO CIP-PMT-DT
                                      COP-PMT-DT.
       MAB-EXIT.
           EXIT.

      *********************************************
       XAA-CALCULATE-APR SECTION.
      *********************************************
      D    STOP MESS.
           MOVE COP-APR-FINANCD TO APR-FINANCED.
           MOVE COP-REG-PAYMENT TO APR-REGPYAMT.
           IF COP-ODD-1ST-PMT = 0
              MOVE COP-TERM TO APR-NOREGPY
              MOVE 0 TO APR-NOODDPY APR-1STPYAMT
           ELSE
              SUBTRACT 1 FROM COP-TERM GIVING APR-NOREGPY
              MOVE 1 TO APR-NOODDPY
              MOVE COP-ODD-1ST-PMT TO APR-1STPYAMT.

      * *****************************************************
      * MJD TESTING FOR A NEW MEXICO CHANGE - MUST ALLOW
      *     VALUE ADDED TAX (VAT)
      *     TO BE INCLUDED IN APR CALC WORLD #582
      * RESORTED BACK TO EXCLUDING VAT TAX AS PER REQUEST 811 07/2012
      *  THIS IS COMBINED WITH A NEW APR CALC FOR MX
      * VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
           IF SP-MFFRMLA-05
              IF APR-1STPYAMT NOT = 0
                 SUBTRACT WKS-MTHLY-MAINT-FEES FROM APR-1STPYAMT
              END-IF
              SUBTRACT WKS-MTHLY-MAINT-FEES FROM APR-REGPYAMT.
      * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
           MOVE COP-IN-DT TO APR-INTDATE.
           MOVE COP-PMT-DT TO APR-1STPYDATE.
           PERFORM ZCA-APR-REGZ.
           IF SP-MFFRMLA-05
              PERFORM MX-ADJUSTED-APR.

       XAA-EXIT.
           EXIT.

      *****************************************************************
      * MEXICO ADJUSTED APR.  (PR#811)
      * 1. COMPUTE THE NOMINAL PERIODIC RATE (NPR) BY FIRST COMPUTING
      *           THE APR AS USUAL (WITHOUT THE VAT INCLUDED)
      * 2. TAKE THE CALCULATED APR (INDECIMAL FORM I.E. APR /100)
      *           AND DIVIDE BY THE NUMBER OF PAYS PER YEAR.
      * 3. NEXT RECOMPUTE THE APR USING    (1 + NPR)^PPY - 1
      *               WHERE NPR = NOMINAL PERIODIC RATE
      *                     PPY = PAY PERIODS PER YEAR.
      * EVERYTHING ELSE ON THE LOAN IS CALCULATED AS IT ALWAYS HAS BEEN.
      *****************************************************************
       MX-ADJUSTED-APR SECTION.
      *--->EVALUATE APR-UNITPER-CD
      *       WHEN "M" MOVE 12 TO WKS-NPR-PPY
      *       WHEN "S" MOVE 24 TO WKS-NPR-PPY
      *       WHEN "B" MOVE 26 TO WKS-NPR-PPY
      *       WHEN "W" MOVE 52 TO WKS-NPR-PPY
      *       WHEN OTHER GO TO MX-ADJUSTED-APR-EXIT
      *--->END-EVALUATE.
           MOVE 12 TO WKS-NPR-PPY.

           COMPUTE WKS-NPR-R = (APR-APR / WKS-NPR-PPY) / 100.
           COMPUTE APR-APR = ((1 + WKS-NPR-R)**WKS-NPR-PPY - 1)*100.
        MX-ADJUSTED-APR-EXIT.
           EXIT.

      *********************************************
       XBA-CALCULATE-SIMPLE-INTEREST  SECTION.
      *********************************************
      *<LOW RATE>
           IF COP-INT-CHARGE = 0
              MOVE .005 TO APR-APR
           ELSE
              PERFORM APRS-GET-SIMPLE-RATE.

      *       MOVE COP-INT-CHARGEABLE TO APR-FINANCED
      *       COMPUTE APR-REGPYAMT ROUNDED =
      *          (COP-INT-CHARGEABLE + COP-INT-CHARGE) / COP-TERM
      *       MOVE COP-TERM TO APR-NOREGPY
      *       MOVE 0 TO APR-NOODDPY APR-1STPYAMT
      *       MOVE COP-IN-DT TO APR-INTDATE
      *       MOVE COP-PMT-DT TO APR-1STPYDATE
      *       PERFORM ZCA-APR-REGZ.

       XBA-EXIT.
           EXIT.


      *XCA-ADDON-WITH-EXTENSION  SECTION.
      *    MOVE 0 TO INT-LO.
      *    MOVE 49.99 TO INT-HI.

      *XCA-LOOP.
      *    IF INT-LO NOT < INT-HI GO TO XCA-SET-RATE.
      *    COMPUTE INT-MID = (INT-LO + INT-HI) / 2.
      *    PERFORM XCA-TRY-RATE.
      *    IF INT-VALUE POSITIVE
      *       SUBTRACT .01 FROM INT-MID GIVING INT-HI
      *       GO TO XCA-LOOP
      *    ELSE
      *       IF INT-VALUE NEGATIVE
      *          ADD .01 INT-MID GIVING INT-LO
      *          GO TO XCA-LOOP.

      *XCA-SET-RATE.
      *    MOVE INT-MID TO COP-EFF-RATE CMP-EFF-RATE.
      *    GO TO XCA-EXIT.

      *XCA-TRY-RATE.
      *    COMPUTE INT-VALUE ROUNDED =
      *      (1 + INT-MID / 36000 * INT-DAYS)
      *      * (1 + INT-MID / 1200) ** INT-MTHS-POS
      *      * (1 + COP-TERM * INT-MID / 1200)
      *      - (1 + COP-INT-CHARGE / COP-INT-CHARGEABLE).

      *XCA-EXIT.
      *    EXIT.


      *XDA-DISCOUNT-WITH-EXTENSION  SECTION.
      *    MOVE 0 TO INT-LO.
      *    MOVE 49.99 TO INT-HI.

      *XDA-LOOP.
      *    IF INT-LO NOT < INT-HI GO TO XDA-SET-RATE.
      *    COMPUTE INT-MID = (INT-LO + INT-HI) / 2.
      *    PERFORM XDA-TRY-RATE.
      *    IF INT-VALUE POSITIVE
      *       SUBTRACT .01 FROM INT-MID GIVING INT-HI
      *       GO TO XDA-LOOP
      *    ELSE
      *       IF INT-VALUE NEGATIVE
      *          ADD .01 INT-MID GIVING INT-LO
      *          GO TO XDA-LOOP.

      *XDA-SET-RATE.
      *    MOVE INT-MID TO COP-EFF-RATE CMP-EFF-RATE.
      *    GO TO XDA-EXIT.

      *XDA-TRY-RATE.
      *    COMPUTE INT-VALUE ROUNDED =
      *      (1 + INT-MID / 36000 * INT-DAYS)
      *      * (1 + INT-MID / 1200) ** INT-MTHS-POS
      *      / (1 - COP-TERM * INT-MID / 1200)
      *      - (1 + COP-INT-CHARGE / COP-INT-CHARGEABLE).

      *XDA-EXIT.
      *    EXIT.


      *********************************************
       ZAA-INITIALIZE  SECTION.
      *********************************************
      
      * VERIFY FOR MIP INSURANCE:
      *   - ONLY ALLOW CREDIT LIFE AND/OR A&H
      *   - DON'T ALLOW A&H UNLESS CREDIT LIFE IS WRITTEN
      
           IF (SP-INS-MIP(5) AND CIP-INS-AH)
              IF NOT (CIP-INS-LL OR CIP-INS-DL)
                 MOVE
           "WHEN MIP, CREDIT LIFE MUST BE WRITTEN WITH A&H" TO MESS
                 PERFORM SEND-MESS
                 MOVE "B" TO COP-RETURN-CODE
                 GO TO ZAA-EXIT.
           IF ( SP-INS-MIP(1) OR SP-INS-MIP(2) OR
                SP-INS-MIP(3) OR SP-INS-MIP(4)
              )
              IF (CIP-INS-LL OR CIP-INS-DL)
                 IF (CIP-INS-PP)  OR
                    (CIP-INS-PP2) OR
                    (CIP-INS-O1)  OR
                    ( (CIP-INS-CO(5) NOT = 0) AND
                                  (NOT CIP-INS-USE-IS-MEMO(5)) ) OR
                    ( (CIP-INS-CO(6) NOT = 0) AND
                                  (NOT CIP-INS-USE-IS-MEMO(6)) )
                    MOVE
           "WHEN MIP, ONLY 'CL' AND 'AH' CAN BE WRITTEN" TO MESS
                    PERFORM SEND-MESS
                    MOVE "B" TO COP-RETURN-CODE
                    GO TO ZAA-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SEE IF THERE WILL BE TAX ON INSURANCE PREMIUMS
      *    NOTE:
      *          CD-CCY-TYPE = " " -> INDICATES NO CITY SALES TAX
      *          CD-CCT-TYPE = " " -> INDICATES NO COUNTY SALES TAX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              MOVE "CY"           TO CD-CCY-TYPE
              MOVE BW-STATE       TO CD-CCY-STATE
              MOVE BW-CITY-TAX-CD TO CD-CCY-CITY
              MOVE " "            TO CD-CCY-FILLER
              MOVE "CT"           TO CD-CCT-TYPE
              MOVE BW-STATE       TO CD-CCT-STATE
              MOVE BW-COUNTY      TO CD-CCT-COUNTY
              MOVE " "            TO CD-CCT-FILLER
              MOVE "CL/INSTAX" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CD-CCY-REC CD-CCT-REC
              CANCEL FORM-PROGX
           ELSE
              MOVE " " TO CD-CCY-REC CD-CCT-REC.

           PERFORM ZBA-HANDLE-INSURANCE.

           IF CIP-PROCEEDS-SW NOT = COP-PROCEEDS-SW
              IF CIP-PROCEEDS-ARE-INPUT
                 MOVE "C" TO CIP-REG-PAYMENT-SW
              ELSE
                 MOVE "I" TO CIP-REG-PAYMENT-SW
           ELSE
              IF CIP-REG-PAYMENT-SW NOT = COP-REG-PAYMENT-SW
                 IF CIP-REG-PAYMENT-IS-INPUT
                    MOVE "C" TO CIP-PROCEEDS-SW
                 ELSE
                    MOVE "I" TO CIP-PROCEEDS-SW
              ELSE
                 NEXT SENTENCE.

           IF SP-SCFRMLA-POINTS
              IF CIP-POINTS-SW NOT = COP-POINTS-SW
                 IF CIP-POINTS-ARE-INPUT
                    MOVE "C" TO CIP-SERVICE-CHG-SW
                 ELSE
                    MOVE "I" TO CIP-SERVICE-CHG-SW
              ELSE
                 IF CIP-SERVICE-CHG-SW NOT = COP-SERVICE-CHG-SW
                    IF CIP-SERVICE-CHG-IS-INPUT
                       MOVE "C" TO CIP-POINTS-SW
                    ELSE
                       MOVE "I" TO CIP-POINTS-SW
                 ELSE
                    NEXT SENTENCE
           ELSE
              NEXT SENTENCE.

           MOVE CIP-CALCULATION-INPUT-AREA
                           TO COP-CALCULATION-OUTPUT-AREA.
           ADD 1 TO COP-NO-OF-TIMES-CALLED.
           MOVE " " TO COP-RETURN-CODE.
           MOVE   0 TO CMP-LOOP-COUNT.
           MOVE CMP-COUNTERS TO COP-COUNTERS.
           IF CIP-NO-OF-TIMES-CALLED = 0
              MOVE "I" TO COP-POINTS-SW
              MOVE "C" TO COP-INS-SW(1)
                          COP-INS-SW(2)
                          COP-INS-SW(3)
                          COP-INS-SW(4)
                          COP-SERVICE-CHG-SW
                          COP-MAINT-FEES-SW
                          COP-1ST-PY-EXT-SW.
           IF COP-INS-IS-CALCD(1)
              MOVE " " TO COP-INS-TERM-SW(1)
              MOVE " " TO COP-COVERAGE-SW(1)
              MOVE " " TO COP-PREMIUM-SW(1).
           IF COP-INS-IS-CALCD(2)
              MOVE " " TO COP-INS-TERM-SW(2)
              MOVE " " TO COP-COVERAGE-SW(2)
              MOVE " " TO COP-PREMIUM-SW(2).
           IF COP-INS-IS-CALCD(3)
              MOVE " " TO COP-INS-TERM-SW(3)
              MOVE " " TO COP-COVERAGE-SW(3)
              MOVE " " TO COP-PREMIUM-SW(3).
           IF COP-INS-IS-CALCD(4)
              MOVE " " TO COP-INS-TERM-SW(4)
              MOVE " " TO COP-COVERAGE-SW(4)
              MOVE " " TO COP-PREMIUM-SW(4).
           IF COP-INS-NO-ONE
              MOVE 01 TO COPIX 
              PERFORM ZBD-CLEAR-INSURANCE 4 TIMES
              GO TO ZAA-INSURANCE-DONE.
           IF NOT (COP-INS-LL OR COP-INS-DL)
              MOVE 01 TO COPIX 
              PERFORM ZBD-CLEAR-INSURANCE
           ELSE
              IF COP-INS-DL
                 IF COP-INS-SINGLE
                    MOVE SP-INSFINCHG(1) TO COP-INSFINCHG-SW(1)
                 ELSE
                    MOVE SP-INSFINCHG(2) TO COP-INSFINCHG-SW(1)
              ELSE
                 IF COP-INS-SINGLE
                    MOVE SP-INSFINCHG(3) TO COP-INSFINCHG-SW(1)
                 ELSE
                    MOVE SP-INSFINCHG(4) TO COP-INSFINCHG-SW(1).

           IF NOT COP-INS-AH
              MOVE 02 TO COPIX
              PERFORM ZBD-CLEAR-INSURANCE
           ELSE
              MOVE SP-INSFINCHG(5) TO COP-INSFINCHG-SW(2).

           IF NOT (COP-INS-PP OR COP-INS-PP2)
              MOVE 03 TO COPIX
              PERFORM ZBD-CLEAR-INSURANCE
           ELSE
              IF COP-INS-PP
                 MOVE SP-INSFINCHG(6) TO COP-INSFINCHG-SW(3)
              ELSE
                 MOVE SP-INSFINCHG(7) TO COP-INSFINCHG-SW(3).

           IF NOT COP-INS-O1
              MOVE 04 TO COPIX
              PERFORM ZBD-CLEAR-INSURANCE
           ELSE
              MOVE SP-INSFINCHG(8) TO COP-INSFINCHG-SW(4).

       ZAA-INSURANCE-DONE.
           MOVE COP-SCREEN-FIELDS   TO CMP-SCREEN-FIELDS.
           MOVE COP-WORKING-AMOUNTS TO CMP-WORKING-AMOUNTS.

      *----------------------------------------------------------------*
      * THE FOLLOWING LINE FORCES THE 2 COMPARISON FIELDS DIFFERENT    *
      * SO THAT THE MAINLINE CONTROL LOOP WILL NOT EXIT ON THE 1ST TRY *
      *----------------------------------------------------------------*
           ADD 1 TO CMP-REG-PAYMENT.

      *----------------------------------------------------------------*
      * GET THE ELAPSED MONTHS AND DAYS BETWEEN THE INTEREST DATE AND
      * THE 1ST PAYMENT DATE. THESE ITEMS WILL BE USED IN THE
      * INTERMEDIATE CALCULATIONS OF THE PROGRAM, WHERE EVEN MONTHS
      * WILL BE USED TO CALCULATE THE SIMPLE INTEREST RATE FOR
      * PAYMENT EXTENSION PURPOSES.
      *
      * ALSO GET COP-1ST-PMT-EXTENSION FOR CALCULATION OF
      * FIRST PAYMENT EXTENSION CHARGES
      *----------------------------------------------------------------*
           MOVE SP-PYEXYEARTP TO ELAPSED-YRTYPE.
           MOVE CIP-IN-DT TO NUM-DATE.
           MOVE CIP-PMT-DT TO SYS-DATE NDTE-DATE.
           PERFORM TIMALL.
           MOVE ELAPSED-REM TO COP-1ST-PMT-DAYS.
           MOVE ELAPSED-MONTHS TO COP-1ST-PMT-MTHS.

           MOVE 0 TO COP-1ST-PMT-EXTENSION.
           IF SP-PYEXSPCODE-REMAIN
              IF ELAPSED-REM > 0
                 MOVE ELAPSED-REM TO COP-1ST-PMT-EXTENSION
              END-IF
           ELSE
           IF SP-PYEXSPCODE-REGZ
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO SYS-DATE
              PERFORM TIMALL
              IF ELAPSED-DAYS > 0
                 MOVE ELAPSED-DAYS TO COP-1ST-PMT-EXTENSION.

      *--------------------------------------------------------------------
      *    GET REAL ASKED FOR OVERRIDING RATE FOR ADDON/DISCOUNT RATES
      *--------------------------------------------------------------------
      *    IF (SP-PYEXFRMLA-REG-INCR OR COP-TERM = 1)
      *       IF NOT (
      *                INT-MTHS = 0
      *                   AND INT-DAYS = 0
      *              )
      *          COMPUTE COP-OVR-RATE ROUNDED = COP-OVR-RATE
      *              * (COP-TERM + INT-MTHS + INT-DAYS/30)
      *                / COP-TERM.

      *----------------------------------------------------------------*
      * HERE IS THE SETUP FOR THE CQA MAINTENANCE FEE ROUTINE.         *
      *----------------------------------------------------------------*

      * MFEE CAN ONLY BE CHARGED IF TERM IS 90 DAYS OR MORE

           IF SP-MFFRMLA-01 OR SP-MFFRMLA-03 OR SP-MFFRMLA-04
              MOVE COP-PMT-DT TO NDTE-DATE
              SUBTRACT 1 FROM COP-TERM GIVING NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO SYS-DATE
              MOVE CIP-IN-DT TO NUM-DATE
              MOVE SP-YEARTYPE TO ELAPSED-YRTYPE
              PERFORM TIMALL
              IF ELAPSED-DAYS < 90
                 MOVE "0" TO WKS-CQA-MAINT-FEE-SW.
      * CAN NOT HAVE ANY OPEN LOANS WITH AN EXISTING MFEE
      * (CL/MFEE04 SETS MFEE-RESTRICT TO 50.00 IF OPEN LOANS)
           IF SP-MFFRMLA-04
              IF COP-MFEE-RESTRICT = 50.00
                 MOVE "0" TO WKS-CQA-MAINT-FEE-SW.


           DIVIDE COP-TERM INTO COP-MAINT-FEES
                                     GIVING WKS-MTHLY-MAINT-FEES.

      * HERE IS THE SETUP FOR THE APR ROUTINE. IN THE SECTION EAA SOME *
      * OF THESE PARAMETERS WILL BE CHANGED TO REFLECT THE ODD 1ST PAY.*
      
           MOVE 1 TO APR-PAYFREQ.
      
      * HERE WE CALCULATE THE VALUES USED BY THE PAYMENT EXTENSION     *
      * ROUTINES, IF THEY ARE NEEDED.                                  *
      
           MOVE 0   TO INT-MTHS INT-DAYS INT-MTHS-POS.
           MOVE 999 TO ELAPSED-YRTYPE.
           MOVE COP-IN-DT TO NUM-DATE.
           MOVE COP-PMT-DT TO SYS-DATE.
           PERFORM TIMALL.
           MOVE ELAPSED-REM TO INT-DAYS.
           SUBTRACT 1 FROM ELAPSED-MONTHS GIVING INT-MTHS.
           IF INT-MTHS POSITIVE
              MOVE INT-MTHS TO INT-MTHS-POS.

      *----------------------------------------------------------------*
      *  DONE NOW FOR THE PRIOR LOAN CALCS
      *----------------------------------------------------------------*
           COMPUTE WKS-PRIOR-LOAN-BALANCE =
                         COP-PRBAL + COP-PRINTDUE - COP-PRFINCHGRB
                                                     - COP-PRINSRB.

      * 57 ONLY 2 WITHIN 12 MONTHS (NCSC23)
      * 58 ONLY 1 WITHIN 12 MONTHS (TXSCHG)
           IF SP-SCFRMLA = 23 OR 43 OR 45 OR 55 OR 56 OR 57 OR 58
      * THE COP-RESTRICT-ADJ IS SET IN XONPC0 TO TRIGGER A RESTRICTION
              IF COP-RESTRICT-ADJ = 50.00
                 MOVE "0" TO WKS-CTJ-SVCCHG-SW
                 GO TO ZAA-EXIT.

           IF NOT (SP-SCFRMLA =  2 OR  3  OR 4 OR  7 OR 11
                             OR 12 OR 13 OR 15 OR 16 OR 18 OR 19
                             OR 24 OR 25 OR 26 OR 27 OR 29 OR 30
                             OR 31 OR 32 OR 33 OR 35 OR 36 OR 37
                             OR 39 OR 40 OR 41 OR 42 OR 47 OR 49
                             OR 52 OR 53 OR 54 OR 55 OR 56 OR 59
                  )
              GO TO ZAA-EXIT.

      * THESE 2 FORMULA'S DONT CARE ABOUT STATE OR CLASS BUT NEED
      * TO CHECK 12 MONTH RESTRICTION
           IF SP-SCFRMLA = 52 OR 53
              GO ZAA-CTM-SETUP.

      * IF REFINANCED, THE LEGAL STATE MUST MATCH THAT OF PRIOR LOAN
      * IF REFINANCED, THE CLASSES MUST MATCH, EXCEPT 3, 12, 13, 19, 24, 30
      *                                        35, 37, 39, 40, 47, 49, 55,
      *                                        59
      * SO IF THEY DONT MATCH, IT EXITS HERE AND CAN CHARGE A SERVICE CHARGE,
      * WILL NOT BE RESTRICTED

           MOVE " " TO COP-PRLOAN-RESTRICTED.
           IF SP-ORGST NOT = CIP-PRIOR-LOAN-STATE
             OR ( (SP-SPRCLASS NOT = CIP-PRIOR-LOAN-CLASS) AND
                  (NOT (SP-SCFRMLA = 12 OR 13 OR 3 OR 19 OR 24 OR 30 OR
                   35 OR 37 OR 39 OR 40 OR 47 OR 49 OR 54 OR 55 OR 59))
                )
              OR ( (CIP-SMALLOAN = "N") AND
                                     (SP-SCFRMLA = 12 OR 13 OR 54) )
                 GO TO ZAA-EXIT.


      * IF REFINANCED, THE RESTRICTION IS IN EFFECT IF BOTH
      *                CLASSES ARE SMALL LOANS
           IF SP-SCFRMLA = 39 OR 40 OR 35
              IF CIP-SMALLOAN NOT = "Y"
                 GO TO ZAA-EXIT
              ELSE
                 MOVE CIP-PRIOR-LOAN-CLASS TO LC-CODE
                 PERFORM OPEN-LC1-FILE
                 PERFORM READ-LC1-FILE
                 PERFORM CLOSE-LC1-FILE
                 IF IO-FG NOT = 0
                    GO TO ZAA-EXIT
                 ELSE
                    IF LC-SMALLOAN NOT = "Y"
                       GO TO ZAA-EXIT.

      * IF REFINANCED, THE SCHG CAN ONLY BE MADE ON NEW MONIES.
      *  3 = REST OF TENNESSEE
      * 19 = CITIZENS, TN
      * 24 = GRAND ISLAND, NE
      * 30 = CITIZENS, TN
           IF SP-SCFRMLA = 3 OR 11 OR 19 OR 24 OR 30
              MOVE "1" TO WKS-CTG-NEW-MONEY-SW
              GO TO ZAA-EXIT.

           IF SP-SCFRMLA = 12 OR 13 OR 54
              MOVE 360 TO ELAPSED-YRTYPE
           ELSE
           IF SP-SCFRMLA = 31 OR 32 OR 33 OR 39 OR 40 OR 55
              MOVE 365 TO ELAPSED-YRTYPE
           ELSE
              MOVE SP-YEARTYPE TO ELAPSED-YRTYPE.
           MOVE CIP-PRIOR-LOAN-DATE TO NUM-DATE.
           MOVE CIP-LN-DT           TO SYS-DATE.
           PERFORM TIMALL.

       ZAA-CTD-SETUP.
      * IF REFINANCED WITHIN 120 DAYS AND ORIGINAL LEGAL STATE AND CLASS
      * ARE EQUAL TO THOSE OF PRIOR LOAN, THE SCHG CAN ONLY BE MADE ON NEW
      * MONIES.
           IF NOT (SP-SCFRMLA = 2 OR 35)
              GO TO ZAA-CTG-SETUP.
           IF (ELAPSED-DAYS NOT < 0) AND (ELAPSED-DAYS < 120)
              MOVE "1" TO WKS-CTD-NEW-MONEY-SW.
           GO TO ZAA-EXIT.

       ZAA-CTG-SETUP.
      * IF REFINANCED AND ORIG LEGAL STATE IS EQUAL TO THOSE OF PRIOR LOAN,
      * THE SCHG CAN ONLY BE MADE ON NEW MONIES.
      * IF REFINANCED WITHIN 45 DAYS, USE HALF
      * 17 = WORLD, TN
      * 60 = WORLD, TN
      * 61 = WORLD, TN
           IF NOT (SP-SCFRMLA = 17 OR 60 OR 61)
              GO TO ZAA-CTJ-SETUP.

           MOVE "1" TO WKS-CTG-NEW-MONEY-SW.
           IF (ELAPSED-DAYS NOT < 0) AND (ELAPSED-DAYS < 45)
              MOVE "1" TO WKS-CTG-USE-HALF-SW.

           GO TO ZAA-EXIT.

       ZAA-CTJ-SETUP.
      * IF REFINANCED WITHIN 12 MONTHS, NO CHARGE CAN BE ASSESSED
      * THIS FORMULA IS EXACTLY LIKE 4, EXCEPT IT DOES NOT MATTER THE
      * LAST TIME A CHARGE WAS ACTUALLY TAKEN, JUST THE 12 MONTH TEST !
           IF NOT (SP-SCFRMLA = 27)
              GO TO ZAA-CTM-SETUP.
           IF ELAPSED-MONTHS < 12
              MOVE "0" TO WKS-CTJ-SVCCHG-SW.
           GO TO ZAA-EXIT.

       ZAA-CTM-SETUP.
      * CASC16 CALLED FROM XONPC0, MOVE 50.00 TO RESTRICT-ADJ IF NO
      *        SERVICE CHARGE ALLOWED (MULLEN AND COSMO AND WORLD)
      * NCSC23 CALLED FROM XONPC0, MOVE 50.00 TO RESTRICT-ADJ IF NO
      *        SERVICE CHARGE ALLOWED (WORLD, 56)
      * TXSCHG CALLED FROM XONPC0, MOVE 50.00 TO RESTRICT-ADJ IF NO
      *        SERVICE CHARGE ALLOWED (WORLD, 58)
           IF NOT (SP-SCFRMLA = 4 OR 16 OR 29 OR 47 OR 49
                                    OR 52 OR 53 OR 56 OR 58 OR 59)
              GO TO ZAA-CTS-SETUP.

           IF COP-RESTRICT-ADJ = 50.00
              MOVE "0" TO WKS-CTJ-SVCCHG-SW.

           GO TO ZAA-EXIT.

       ZAA-CTS-SETUP.
           IF SP-SCFRMLA NOT = 7
              GO TO ZAA-CTU-SETUP.
           IF ELAPSED-MONTHS < 12
              MOVE "0" TO WKS-CTS-SVCCHG-SW.
           GO TO ZAA-EXIT.

      *****************************************************
      * NOTE:
      *      A COPY OF THIS LOGIC EXISTS IN LIBLP/LPAVCR
      *      IF YOU CHANGE (CHANGE     B O T H).
       ZAA-CTU-SETUP.
           IF NOT (SP-SCFRMLA = 12 OR 13 OR 54)
              GO TO ZAA-CTV-SETUP.
           IF (SP-SCFRMLA = 13 AND COP-SOURCE-CD = "SC")
              GO TO ZAA-EXIT.
           MOVE WKS-PRIOR-LOAN-BALANCE TO WKS-TOTPRIOR-LOAN-BALANCE.
           IF COP-PRPAID-AHEAD > 0
              ADD COP-PRPAID-AHEAD TO WKS-TOTPRIOR-LOAN-BALANCE.
      *   ----------------------------------------------------
      *   TEST FOR NEW GILA LAW
      *   IF LOAN UNDER CONSIDERATION FOR RESTRICTION WAS MADE
      *   ON OR PRIOR TO 041301 USE $200.00 ELSE $300.00
      *   ----------------------------------------------------
           IF CIP-PRIOR-LOAN-DATE NOT > 20010413
              MOVE 200.00 TO WKS-CTU-AMOUNT
           ELSE
              MOVE 300.00 TO WKS-CTU-AMOUNT.
           IF WKS-TOTPRIOR-LOAN-BALANCE <= WKS-CTU-AMOUNT
              IF ELAPSED-DAYS < 61
                 MOVE "1" TO WKS-CTU-NEW-MONEY-SW
                 MOVE "Y" TO COP-PRLOAN-RESTRICTED
              ELSE
                 NEXT SENTENCE
           ELSE
              IF ELAPSED-DAYS < 181
                 MOVE "1" TO WKS-CTU-NEW-MONEY-SW
                 MOVE "Y" TO COP-PRLOAN-RESTRICTED.
           GO TO ZAA-EXIT.

      * IF REFINANCED WITHIN 6 MONTHS AND ORIGINAL LEGAL STATE AND CLASS
      * ARE EQUAL TO THOSE OF PRIOR LOAN, THE SCHG CAN ONLY BE MADE ON NEW
      * MONIES. MERCURY, 'KS'
       ZAA-CTV-SETUP.
           IF NOT (SP-SCFRMLA = 25 OR 36 OR 41)
              GO TO ZAA-CTW-SETUP.
           IF SP-SCFRMLA = 41
              IF ELAPSED-MONTHS < 12
                 MOVE "0" TO WKS-CTS-SVCCHG-SW
              END-IF
           ELSE
              IF ELAPSED-MONTHS < 6
                 MOVE "0" TO WKS-CTS-SVCCHG-SW.
           GO TO ZAA-EXIT.

      * IF REFINANCED WITHIN 4 MONTHS AND ORIGINAL LEGAL STATE AND CLASS
      * ARE EQUAL TO THOSE OF PRIOR LOAN, THE SCHG CAN ONLY BE MADE ON NEW
      * MONIES. MERCURY, 'WA'
       ZAA-CTW-SETUP.
           IF SP-SCFRMLA NOT = 26
              GO TO ZAA-CTT-31-32-33-SETUP.
           IF ELAPSED-MONTHS < 4
              MOVE "0" TO WKS-CTJ-SVCCHG-SW.
           GO TO ZAA-EXIT.

      * IF REFINANCED WITHIN 90 DAYS AND ORIGINAL LEGAL STATE AND CLASS
      * ARE EQUAL TO THOSE OF PRIOR LOAN, NO SERCHG CAN BE CHARGED
      * REGENCY OHIO, 'OH'

      * IF FRMLA 33, IF REFINANCED BETWEEN 91 AND A YEAR, NO POINTS CAN
      * BE TAKEN, BUT THE ORIGINATION FEE MAY BE CHARGED
       ZAA-CTT-31-32-33-SETUP.
           IF NOT (SP-SCFRMLA = 31 OR 32 OR 33 OR 39 OR 40 OR 55)
              GO TO ZAA-CTT-37-SETUP.
           IF ELAPSED-DAYS < 91
              MOVE "0" TO WKS-CTJ-SVCCHG-SW.
           IF SP-SCFRMLA = 33
              IF ELAPSED-DAYS > 90 AND ELAPSED-DAYS < 366
                 MOVE "2" TO WKS-CTJ-SVCCHG-SW.
           GO TO ZAA-EXIT.

      * IF REFINANCED WITHIN 180 DAYS AND ORIGINAL LEGAL STATE AND CLASS
      * ARE EQUAL TO THOSE OF PRIOR LOAN, NO CHARGE CAN BE ASSESSED.
      * EXCEL, 'TX'
       ZAA-CTT-37-SETUP.
           IF SP-SCFRMLA NOT = 37
              GO TO ZAA-EXIT.

           IF ELAPSED-DAYS < 181 AND COP-SOURCE-CD NOT = "SC"
              MOVE "0" TO WKS-CTJ-SVCCHG-SW.

       ZAA-EXIT.
           EXIT.


      ***********************************
       ZBA-HANDLE-INSURANCE  SECTION.
      ***********************************
           MOVE 01 TO CIPIX COPIX. 
           PERFORM ZBA-SET-INS-SWS 8 TIMES.
           GO TO ZBA-EXIT.

       ZBA-EXIT.
           EXIT.

      ***********************************
       ZBA-SET-INS-SWS SECTION.
      ***********************************
           IF COP-INS-TERM(CIPIX) NOT = 202
              IF CIP-INS-TERM(CIPIX) NOT = COP-INS-TERM(COPIX)
                 MOVE "C" TO CIP-INS-TERM-SW(CIPIX).
           IF COP-INS-COVERAGE(CIPIX) NOT = 2020202.02
              IF CIP-INS-COVERAGE(CIPIX) NOT = COP-INS-COVERAGE(COPIX)
                 MOVE "C" TO CIP-COVERAGE-SW(CIPIX).
           IF COP-INS-PREMIUM(CIPIX) NOT = 2020202.02
              IF CIP-INS-PREMIUM(CIPIX) NOT = COP-INS-PREMIUM(COPIX)
                 MOVE "C" TO CIP-PREMIUM-SW(CIPIX).
           ADD 01 TO CIPIX COPIX. 

      ***********************************
       ZBD-CLEAR-INSURANCE  SECTION.
      ***********************************
           MOVE " " TO COP-INSURANCES(COPIX).
           MOVE 0   TO COP-INS-CO(COPIX)
                       COP-INS-TERM(COPIX)
                       COP-INS-COVERAGE(COPIX)
                       COP-INS-PREMIUM(COPIX)
                       COP-INS-REJECT-TERM(COPIX)
                       COP-INS-TAX(COPIX).
           MOVE " " TO COP-INSFINCHG-SW(COPIX).
           ADD  01 TO COPIX.

       ZBD-EXIT.
           EXIT.

      ***********************************
       ZCA-APR-REGZ SECTION.
      ***********************************
           MOVE WKS-SPAY-FG TO APR-SPAY-FG.
           PERFORM APR-REGZ.

       ZCA-EXIT.
           EXIT.

      ***********************************
       GET-ALL-INS-PREMIUMS SECTION.
      ***********************************
           MOVE 0 TO WKS-ALL-INS-PREMIUMS
                     WKS-ALL-INS-TAX.

           IF SP-INS-MIP(1) OR SP-INS-MIP(2) OR
              SP-INS-MIP(3) OR SP-INS-MIP(4) OR
              SP-INS-MIP(5)
              GO TO GET-ALL-INS-PREMIUMS-EXIT.

           IF NOT COP-INS-USE-IS-MEMO(1)
              ADD COP-INS-TAX(1)     TO WKS-ALL-INS-TAX
              ADD COP-INS-PREMIUM(1) TO WKS-ALL-INS-PREMIUMS.
           IF NOT COP-INS-USE-IS-MEMO(2)
              ADD COP-INS-TAX(2)     TO WKS-ALL-INS-TAX
              ADD COP-INS-PREMIUM(2) TO WKS-ALL-INS-PREMIUMS.
           IF NOT COP-INS-USE-IS-MEMO(3)
              ADD COP-INS-TAX(3)     TO WKS-ALL-INS-TAX
              ADD COP-INS-PREMIUM(3) TO WKS-ALL-INS-PREMIUMS.
           IF NOT COP-INS-USE-IS-MEMO(4)
              ADD COP-INS-TAX(4)     TO WKS-ALL-INS-TAX
              ADD COP-INS-PREMIUM(4) TO WKS-ALL-INS-PREMIUMS.
           IF NOT COP-INS-USE-IS-MEMO(5)
              ADD COP-INS-TAX(5)     TO WKS-ALL-INS-TAX
              ADD COP-INS-PREMIUM(5) TO WKS-ALL-INS-PREMIUMS.
           IF NOT COP-INS-USE-IS-MEMO(6)
              ADD COP-INS-TAX(6)     TO WKS-ALL-INS-TAX
              ADD COP-INS-PREMIUM(6) TO WKS-ALL-INS-PREMIUMS
           ELSE
           IF COP-INS-USE-IS-MEMO(6)
              ADD COP-INS-TAX(6)     TO WKS-ALL-INS-TAX.
           IF NOT COP-INS-USE-IS-MEMO(7)
              ADD COP-INS-TAX(7)     TO WKS-ALL-INS-TAX
              ADD COP-INS-PREMIUM(7) TO WKS-ALL-INS-PREMIUMS.
           IF NOT COP-INS-USE-IS-MEMO(8)
              ADD COP-INS-TAX(8)     TO WKS-ALL-INS-TAX
              ADD COP-INS-PREMIUM(8) TO WKS-ALL-INS-PREMIUMS.

       GET-ALL-INS-PREMIUMS-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRZL.CPY".
      ***********************************
       ERROR-DISPLAY SECTION.
      ***********************************
      *      MOVE "COP-CMP-OVR-RATE = " TO MESS-MESS.
      *      MOVE COP-OVR-RATE TO MESS-AMT.
      *      MOVE CMP-OVR-RATE TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-EFF-RATE = " TO MESS-MESS.
      *      MOVE COP-EFF-RATE TO MESS-AMT.
      *      MOVE CMP-EFF-RATE TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-SIM-RATE = " TO MESS-MESS.
      *      MOVE COP-SIM-RATE TO MESS-AMT.
      *      MOVE CMP-SIM-RATE TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-APR-RATE = " TO MESS-MESS.
      *      MOVE COP-APR-RATE TO MESS-AMT.
      *      MOVE CMP-APR-RATE TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-SERVICE-CHG = " TO MESS-MESS.
      *      MOVE COP-SERVICE-CHG TO MESS-AMT.
      *      MOVE CMP-SERVICE-CHG TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-MAINT-FEES = " TO MESS-MESS.
      *      MOVE COP-MAINT-FEES TO MESS-AMT.
      *      MOVE CMP-MAINT-FEES TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-1ST-PY-EXT = " TO MESS-MESS.
      *      MOVE COP-1ST-PY-EXT TO MESS-AMT.
      *      MOVE CMP-1ST-PY-EXT TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-INT-CHARGE = " TO MESS-MESS.
      *      MOVE COP-INT-CHARGE TO MESS-AMT.
      *      MOVE CMP-INT-CHARGE TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-AMT-FINANCD = " TO MESS-MESS.
      *      MOVE COP-AMT-FINANCD TO MESS-AMT.
      *      MOVE CMP-AMT-FINANCD TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-LOAN-AMOUNT = " TO MESS-MESS.
      *      MOVE COP-LOAN-AMOUNT TO MESS-AMT.
      *      MOVE CMP-LOAN-AMOUNT TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-TOT-REPYBLE = " TO MESS-MESS.
      *      MOVE COP-TOT-REPYBLE TO MESS-AMT.
      *      MOVE CMP-TOT-REPYBLE TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-ANTIC-EARNS = " TO MESS-MESS.
      *      MOVE COP-ANTIC-EARNS TO MESS-AMT.
      *      MOVE CMP-ANTIC-EARNS TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-ODD-1ST-PMT = " TO MESS-MESS.
      *      MOVE COP-ODD-1ST-PMT TO MESS-AMT.
      *      MOVE CMP-ODD-1ST-PMT TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-REG-PAYMENT = " TO MESS-MESS.
      *      MOVE COP-REG-PAYMENT TO MESS-AMT.
      *      MOVE CMP-REG-PAYMENT TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      MOVE "COP-CMP-PROCEEDS = " TO MESS-MESS.
      *      MOVE COP-PROCEEDS TO MESS-AMT.
      *      MOVE CMP-PROCEEDS TO MESS-AMT2.
      *      PERFORM SEND-MESS.

      *      IF COP-INPUT-FIELDS NOT = CMP-INPUT-FIELDS
      *         MOVE "COP-INPUT-FIELDS NOT = " TO MESS-MESS
      *         PERFORM SEND-MESS.

      *      IF COP-INTERMED-FIELDS NOT = CMP-INTERMED-FIELDS
      *         MOVE "COP-INTERMED-FIELDS NOT = " TO MESS-MESS
      *         PERFORM SEND-MESS.

      *      IF COP-OUTPUT-FIELDS NOT = CMP-OUTPUT-FIELDS
      *         MOVE "COP-OUTPUT-FIELDS NOT = " TO MESS-MESS
      *         PERFORM SEND-MESS.

      ********************************
       IO-ROUTINES SECTION.
      ********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       EXIT-PROG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATH BEFORE GOING INTO
      *    'CLOSE-FILES' (LIBGB/*DECLARE*) SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CALCZL"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM CLOSE-FILES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXIT PROGRAM.
