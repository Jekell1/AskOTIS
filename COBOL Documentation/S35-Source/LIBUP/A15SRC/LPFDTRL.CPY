      * COPYMEMBER: LIBLP/LPFDTRL
      **************************************************************************
      *               TR-CODE  =  "L"
      *                 DIRECT LOAN
      *
      *  DESC: DIRECT LOAN TRANSACTION RECORD
      * REV:
      *  JTG 062795 ADDED TL-POSTDATE, TL-POSTTIME, TL-USERID
      *  JTG 080495 CORRECTED TL-INSURED & TL-INSELIGIBLE
      *             FOR O3 INSURECNE CALCULATED
      *  JTG 082097 ADDED TL-GIFTNET & TL-APR WORLD A90
      *  BAH 062598 CHANGED TL-TILLNO PIC 9 TO PIC X
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  JTG 990322 ADDED TL-SP-LNCOST-FRMLA RE: TFC #008
      *  JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  CS  021121 ADDED INS-TAX-COUNTY-CD INS-TAX-CITY-CD FOR TAXES ON 
      *      INSURANCE WRITTEN [WORLD] 
      *  BLV 021218 ADDED 88 FOR TL-FEECODE-INS-TAX TO IDENTIFY FEE CODES
      *             FOR SALES TAX ON INSURANCE [WORLDPR#288]
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      *  BAH 130801 ADDED TL-POINTS FOR EMLNRG, WORLD PR# 898
      *  BAH 180215 ADDED TL-DEBIT-CARD-AMT FOR NETSPEND WORLD PR#1306
      **************************************************************************
       01  TL-REC.
           03  TL-DAY              PIC 99         COMP.
           03  TL-BRNO             PIC 9(4)       COMP.
           03  TL-CODE             PIC X.
           03  TL-CLASS            PIC 99         COMP.
           03  TL-NUMBER           PIC 9(6)       COMP.
           03  TL-SEQNO            PIC 99         COMP.
           03  TL-LAWCODE          PIC 99         COMP.
           03  TL-LINE             PIC 99         COMP.
           03  TL-FILLER2          PIC X(3).
      *----------------------------------------------------
           03  TL-BORRTBL          OCCURS 4 TIMES.
               05  TL-SSNO         PIC 9(9)       COMP-3.
               05  TL-MAKERCD      PIC X.
      *----------------------------------------------------
           03  TL-PURCD            PIC XXX.
           03  TL-SRCD             PIC XX.
           03  TL-COLLCD           PIC XX.
           03  TL-SERVACCT         PIC X.
           03  TL-LOANTYPE         PIC X.
      *----------------------------------------------------
           03  TL-LOANDATE         PIC 9(6)       COMP-3.
           03  TL-INTDATE          PIC 9(6)       COMP-3.
      *----------------------------------------------------
           03  TL-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TL-LNAMT            PIC S9(7)V99   COMP-3.
           03  TL-FINCHG           PIC S9(7)V99   COMP-3.
           03  TL-INTCHG           PIC S9(7)V99   COMP-3.
           03  TL-SERCHG           PIC S9(5)V99   COMP-3.
           03  TL-EXTCHG           PIC S9(5)V99   COMP-3.
           03  TL-MAINTFEE         PIC S999V99    COMP-3.
           03  TL-ORGTERM          PIC 9(3)       COMP-3.
           03  TL-1STPYAMT         PIC S9(7)V99   COMP-3.
           03  TL-NOREGPY          PIC 9(3)       COMP-3.
           03  TL-REGPYAMT         PIC S9(7)V99   COMP-3.
           03  TL-LASTPYAMT        PIC S9(7)V99   COMP-3.
           03  TL-PROCASH          PIC S9(7)V99   COMP-3.
           03  TL-TILLNO           PIC X.
      *----------------------------------------------------
           03  TL-FEETBL           OCCURS 10 TIMES.
               05  TL-FEECODE      PIC XX.
                   88 TL-FEECODE-INS-TAX  VALUE "TC" "TA" "TP" "T1"
                                                "T2" "T3" "T4" "T5".
               05  TL-FEEAMT       PIC S9(5)V99   COMP-3.
      *----------------------------------------------------
           03  TL-INSCODES.
               05  TL-INSURED          PIC X.
               05  TL-INSTYPES         PIC X.
                   88  TL-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                 "0" "1" "2" "3" "4"
                                                 "5" "6" "7" "8" "9".
                   88  TL-INSTYPES-DL      VALUE "C"
                                                 "G" "I" "J" "M" "P"
                                                 "T" "X" "Y" "5" "6"
                                                 "7".
                   88  TL-INSTYPES-LL      VALUE "B"
                                                 "A" "F" "H" "L" "O"
                                                 "S" "U" "W" "2" "3"
                                                 "4".
                   88  TL-INSTYPES-AH      VALUE "D"
                                                 "F" "G" "K" "L" "M"
                                                 "O" "P" "Q" "V" "W"
                                                 "X" "3" "4" "6" "7"
                                                 "9" "0".
                   88  TL-INSTYPES-PP      VALUE "E"
                                                 "H" "J" "K" "L" "M"
                                                 "U" "Y" "Z" "3" "6"
                                                 "9".
                   88  TL-INSTYPES-PP2     VALUE "R"
                                                 "S" "T" "V" "W" "X"
                                                 "2" "4" "5" "7" "8"
                                                 "0".
                   88  TL-INSTYPES-O1      VALUE "1"
                                                 "A" "I" "O" "P" "Q"
                                                 "U" "Y" "Z" "2" "3"
                                                 "4" "5" "6" "7" "8"
                                                 "9" "0".
               05  TL-INSELIGIBLE  PIC X.                  
                   88  TL-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  TL-INSELIGIBLE-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  TL-INSELIGIBLE-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  TL-INSELIGIBLE-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  TL-INSELIGIBLE-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  TL-INSELIGIBLE-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  TL-INSELIGIBLE-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
           03  TL-INSTBL           OCCURS 10 TIMES.
               05  TL-INSCOMP      PIC 99.
               05  TL-INSOURS      PIC X.
               05  TL-INSEFF       PIC 9(6)       COMP-3.
               05  TL-INSTERM      REDEFINES TL-INSEFF
                                   PIC 9(6)       COMP-3.
               05  TL-INSEXP       PIC 9(6)       COMP-3.
               05  TL-INSPREM      PIC S9(5)V99   COMP-3.
               05  TL-INSCOMM      PIC S9(6)V99   COMP-3.
      *----------------------------------------------------
           03  TL-DLNO             PIC 9(4)V99    COMP-3.
      *----------------------------------------------------
           03  TL-EARNINGS         OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
           03  TL-ANTICERN         OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TL-PRLNNO           PIC 9(8)       COMP-3.
           03  TL-PRBAL            PIC S9(6)V99   COMP-3.
           03  TL-PRINTDUE         PIC S9(5)V99   COMP-3.
           03  TL-PRFINCHGRB       PIC S9(5)V99   COMP-3.
           03  TL-PRINSRB          PIC S9(5)V99   COMP-3.
      *----------------------------------------------------
      * NOTE: THIS FLAG IDENTIFIES HOW LPEOD5.C SHOULD
      *       UPDATE G/L WITH TL-STATE-WORKER:
      *       A - MISSISSIPPI REBL01.C
           03  TL-LPEOD5-FG        PIC X.
           03  TL-STATE-WORKER     PIC S9(7)V99   COMP-3.
           03  TL-RESTRICTD-ACCTNO PIC 9(8)       COMP-3.
           03  TL-SPRCLASS         PIC 99.
           03  TL-USERID           PIC X(10).
           03  TL-POSTDATE         PIC  9(6)     COMP-3.
           03  TL-POSTTIME         PIC  9(6)     COMP-3.
           03  TL-GIFTNET          PIC X.
           03  TL-APR              PIC 9(3)V999  COMP-3.
           03  TL-SP-LNCOST-FRMLA  PIC X.
1801       03  TL-MIP-FG           PIC X.
           03  TL-INS-TAX-COUNTY-CD PIC X(3).
           03  TL-INS-TAX-CITY-CD   PIC X(3).
2287       03  TL-PRDLDISC-UNEARN   PIC S9(7)V99   COMP-3.
           03  TL-POINTS            PIC 99V99      COMP-3.
           03  TL-DEBIT-CARD-AMT    PIC S9(7)V99   COMP-3.
           03  TL-ACTIONCD2         PIC X(4).
           
           03  FILLER               PIC X(29).
