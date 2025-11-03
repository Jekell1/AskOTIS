      * COPYMEMBER: LIBLP/LPFDTRS
      ************************************************************************
      *               TR-CODE  =  "S"
      *           SALES FINANCE NEW INPUT
      * REV:
      *  JTG 040295 EXPANDED TS-FEETBL FROM 6 TO 10
      *  JTG 061995 ADDED TS-O3INS-DLPROC
      *  MJD 062795 ADDED TS-USERID, TS-POSTDATE, TS-POSTTIME.
      *  JTG 071295 REMOVED TS-PR,FIELDS AND ADDED TS-INSPREM-DLADJ
      *  JTG 080495 CORRECTED TS-INSURED & TS-INSELIGIBLE
      *  JTG 071598 ADDED TS-PRDLDISC-UNEARN TO HOLD CARRY OVER DISCOUNT
      *             ON RENEWALS
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  JTG 990322 ADDED TS-SP-LNCOST-FRMLA RE: TFC #008
      *  MJD 990910 CHANGED LN-INSELIGIBLE-O3 TO BE LN-INSELIGIBLE-O1.
      *             AS PER JOHN, IT WAS MISNAMED FROM THE START.
      *  BAH 020614 ADDED TS-DLPAID, REGACC PR# 1822
      *  JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  JTG 020916 ADDED LN-DLCOMM-03-CMSI LN-DLCOMM-PP-CMSI      REGACC #1846
      *             MAKE TS-INSPREM LEVELS 8 LIKE LOAN RECORD
      *             AND CREATED FILLER OF 40 
      *             ADDED TS-DLCOMM-PP-CMSI TS-DLCOMM-O3-CMSI
      *  BLV 021017 DIDN'T CHANGE RECORD, JUST PUT USER INITIALS BACK ON
      *             REVISION LINES
      *  CS  021121 ADDED TS-INS-TAX-COUNTY-CD TS-INS-TAX-CITY-CD FOR TAXING  
      *             AUTHORITY ON INSURANCES WRITTEN [WORLD PR#288] NOT USED
      *  JTG 050923 ADDED TS-TILLNO AND TS-UNWIND-REASON-CODE       REGACC #0105
      **************************************************************************
       01  TS-REC.
           03  TS-DAY              PIC 99         COMP.
           03  TS-BRNO             PIC 9(4)       COMP.
           03  TS-CODE             PIC X.
           03  TS-CLASS            PIC 99         COMP.
           03  TS-NUMBER           PIC 9(6)       COMP.
           03  TS-SEQNO            PIC 99         COMP.
           03  TS-LAWCODE          PIC 99         COMP.
           03  TS-LINE             PIC 99         COMP.
           03  TS-FILLER2          PIC X(3).
      *----------------------------------------------------
           03  TS-BORRTBL         OCCURS 4 TIMES.
               05  TS-SSNO         PIC 9(9)       COMP-3.
               05  TS-MAKERCD      PIC X.
      *----------------------------------------------------
           03  TS-PURCD            PIC XXX.
           03  TS-SRCD             PIC XX.
           03  TS-COLLCD           PIC XX.
           03  TS-SERVACCT         PIC X.
           03  TS-LOANTYPE         PIC X.
      *----------------------------------------------------
           03  TS-LOANDATE         PIC 9(6)       COMP-3.
           03  TS-INTDATE          PIC 9(6)       COMP-3.
      *----------------------------------------------------
           03  TS-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TS-LNAMT            PIC S9(7)V99   COMP-3.
           03  TS-FINCHG           PIC S9(7)V99   COMP-3.
           03  TS-INTCHG           PIC S9(7)V99   COMP-3.
           03  TS-SERCHG           PIC S9(5)V99   COMP-3.
           03  TS-EXTCHG           PIC S9(5)V99   COMP-3.
           03  TS-MAINTFEE         PIC S999V99    COMP-3.
           03  TS-ORGTERM          PIC 9(3)       COMP-3.
           03  TS-1STPYAMT         PIC S9(7)V99   COMP-3.
           03  TS-NOREGPY          PIC 9(3)       COMP-3.
           03  TS-REGPYAMT         PIC S9(7)V99   COMP-3.
           03  TS-LASTPYAMT        PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TS-FEETBL          OCCURS 10 TIMES.
               05  TS-FEECODE      PIC XX.
               05  TS-FEEAMT       PIC S9(5)V99   COMP-3.
               05  TS-FEEOURS      PIC X.
      *----------------------------------------------------
           03  TS-INSCODES.
               05  TS-INSURED      PIC X.
               05  TS-INSTYPES     PIC X.
                   88  TS-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                 "0" "1" "2" "3" "4"
                                                 "5" "6" "7" "8" "9".
                   88  TS-INSTYPES-DL      VALUE "C"
                                                 "G" "I" "J" "M" "P"
                                                 "T" "X" "Y" "5" "6"
                                                 "7".
                   88  TS-INSTYPES-LL      VALUE "B"
                                                 "A" "F" "H" "L" "O"
                                                 "S" "U" "W" "2" "3"
                                                 "4".
                   88  TS-INSTYPES-AH      VALUE "D"
                                                 "F" "G" "K" "L" "M"
                                                 "O" "P" "Q" "V" "W"
                                                 "X" "3" "4" "6" "7"
                                                 "9" "0".
                   88  TS-INSTYPES-PP      VALUE "E"
                                                 "H" "J" "K" "L" "M"
                                                 "U" "Y" "Z" "3" "6"
                                                 "9".
                   88  TS-INSTYPES-PP2     VALUE "R"
                                                 "S" "T" "V" "W" "X"
                                                 "2" "4" "5" "7" "8"
                                                 "0".
                   88  TS-INSTYPES-O1      VALUE "1"
                                                 "A" "I" "O" "P" "Q"
                                                 "U" "Y" "Z" "2" "3"
                                                 "4" "5" "6" "7" "8"
                                                 "9" "0".
               05  TS-INSELIGIBLE  PIC X.                  
                   88  TS-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  TS-INSELIGIBLE-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  TS-INSELIGIBLE-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  TS-INSELIGIBLE-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  TS-INSELIGIBLE-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  TS-INSELIGIBLE-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  TS-INSELIGIBLE-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
1846  *    03  TS-INSTBL          OCCURS 10.
1846       03  TS-INSTBL          OCCURS 8.
               05  TS-INSCOMP      PIC 99.
               05  TS-INSOURS      PIC X.
               05  TS-INSEFF       PIC 9(6)       COMP-3.
               05  TS-INSTERM     REDEFINES TS-INSEFF
                                   PIC 9(6)       COMP-3.
               05  TS-INSEXP       PIC 9(6)       COMP-3.
               05  TS-INSPREM      PIC S9(5)V99   COMP-3.
               05  TS-INSCOMM      PIC S9(6)V99   COMP-3.

1846  *-FREED UP 40 BYTES----------------------------------
1846       03  TS-DLCOMM-PP-CMSI     PIC S9(4)V99   COMP-3.
1846       03  TS-DLCOMM-O3-CMSI     PIC S9(4)V99   COMP-3.
           03  TS-INS-TAX-COUNTY-CD  PIC X(3).
           03  TS-INS-TAX-CITY-CD    PIC X(3).
0105       03  TS-TILLNO             PIC X.
0105       03  TS-UNWIND-REASON-CODE PIC XX.
           03  TS-FILLER             PIC X(23).
      *----------------------------------------------------
           03  TS-DLNO             PIC 9(4)V99    COMP-3.
           03  TS-DLRECOURSE       PIC X.
           03  TS-DLPARTRCAMT      PIC S9(7)V99   COMP-3.
           03  TS-PURBAL           PIC S9(7)V99   COMP-3.
           03  TS-DLRVPRIN        OCCURS 3
                                   PIC S9(7)V99   COMP-3.
           03  TS-DLRVCHGS        OCCURS 3
                                   PIC S9(6)V99   COMP-3.
           03  TS-DLPARVHELD       PIC S9(7)V99   COMP-3.
           03  TS-DLPARVPAID       PIC S9(6)V99   COMP-3.
           03  TS-DLDISC           PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TS-ANTICTERM        PIC 9(3)       COMP-3.
           03  TS-ANTICERN        OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
           03  TS-EARNINGS        OCCURS 6 TIMES
                                   PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TS-PRLNNO           PIC 9(8)       COMP-3.
           03  TS-INSPREM-DLADJ    PIC S9(5)V99   COMP-3.
           03  TS-PRDLDISC-UNEARN  PIC S9(7)V99   COMP-3.
           03  TS-SP-LNCOST-FRMLA  PIC X.
           03  TS-DLPAID           PIC X.
1801       03  TS-MIP-FG           PIC X.
           03  TS-ACTIONCD2        PIC X(4).
           03  FILLER              PIC X.
      *----------------------------------------------------
           03  TS-DL-RVCHGS-CODE  OCCURS 3
                                   PIC X.
           03  TS-DL-PARVHELD-CODE PIC X.
           03  TS-DL-PARVPAID-CODE PIC X.
           03  TS-SPRCLASS         PIC 99.
           03  TS-O3INS-DLPROC     PIC S9(6)V99   COMP-3.
           03  TS-USERID           PIC X(10).
           03  TS-POSTDATE         PIC 9(6)       COMP.
           03  TS-POSTTIME         PIC 9(6)       COMP.
