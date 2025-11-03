      * COPYMEMBER: LIBLP/LPFDTRE.CPY
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE [REVOLVING SALES FINANCE]
      *
      * FILE INFO  :  PRIMARY LAYOUT IN LIBLP/LPFDTR.CPY
      *               TR1-CODE = E - REVOLVING SALES FINANCE NEW INPUT
      *               LOCAL ED/TR###  (####=MMYY)              RSZ=950
      *
      *=================================================================
      * REV:
      * JTG 102898 NEW
      * MJD 990910 CHANGED LN-INSELIGIBLE-O3 TO BE LN-INSELIGIBLE-O1.
      *            AS PER JOHN, IT WAS MISNAMED FROM THE START.
      *  CS 160916 CHANGED TE-CLASS 99 TO 999
      *            ADDED TE-ACCTNO, TE-NUMBER IS NOW REDEFINE OF TE-ACCTNO
      *            REMOVE TE-SEQNO
      *            CHANGE TE-LOANDATE PIC 9(6) TO 9(8)            
      *                   TE-INTDATE 9(6) TO 9(8)
      *                   TE-MAINTFEE S9(3)V99 TO S9(5)V99
      *                   TE-FEETBL OCCURS 10 TO 20 
      *            TE-INSEFF  9(6) TO 9(8)                   
      *            TE-INSTERM 9(6) TO 9(8)
      *            TE-INSEXPT (6) TO 9(8)
      *            TE-INSCOMM 9(6)V99 TO 9(5)V99
      *            TE-DLRVCHGS,TE-DLPARVPAID FROM 9(6)V99 TO 9(7)V99
      *            TE-ANTICERN OCCURS 6 TO 10 TIMES   
      *            TE-EARNINGS OCCURS 6 TO 10 TIMES  
      *            ADD TE-INSANTICERN TO TE-INSTBL
      *                TE-INSEARNINGS TO TE-INSTBL 
      *            SAVED VERSION LPFDTRE.160921
      *
      * KEC 2016.1028 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED '-DATE' TO THE END OF TE-INSEFF AND TE-INSEXP.
      *  CS 161117 ADD TE-SUBCLASS PER KM
      *  CS 170713 CHANGED TE-LINE TO PIC 9(3) TO MATCH WK2-LINE, IN A15
      *            WAS COMP 9(2) WHICH ALLOWED A VALUE OF 248, I DONT THINK
      *            WE ALLOWED LINE >99 BUT CHANGED TO 999. FILLER TO 4
      *            SAVED LPFDTRE.170713
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      ******************************************************************
       01  TE-REC.
           03  TE-TRE-KEY.
               05  TE-DAY              PIC 99.  
               05  TE-BRNO             PIC 9(4). 
               05  TE-CODE             PIC X.
               05  TE-CLASS            PIC 999. 
               05  TE-ACCTNO           PIC 9(8). 
               05  TE-LAWCODE          PIC 99.
               05  TE-LINE             PIC 999.
               05  TE-FILLER           PIC X(4).
      *----------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  TE-LTOUCH-DATE          PIC 9(8)   COMP-3. 

           03  TE-BORRTBL         OCCURS 4 TIMES.
               05  TE-SSNO         PIC 9(9)       COMP-3.
               05  TE-MAKERCD      PIC X.
      *----------------------------------------------------
           03  TE-PURCD            PIC XXX.
           03  TE-SRCD             PIC XX.
           03  TE-COLLCD           PIC XX.
           03  TE-SERVACCT         PIC X.
           03  TE-LOANTYPE         PIC X.
           03  TE-SUBCLASS         PIC 9(2).
      *----------------------------------------------------

      *>>XDD DATE=YYYYMMDD
           03  TE-LOANDATE         PIC 9(8)       COMP-3.

      *>>XDD DATE=YYYYMMDD
           03  TE-INTDATE          PIC 9(8)       COMP-3.
      *----------------------------------------------------
           03  TE-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TE-LNAMT            PIC S9(7)V99   COMP-3.
           03  TE-FINCHG           PIC S9(7)V99   COMP-3.
           03  TE-INTCHG           PIC S9(7)V99   COMP-3.
           03  TE-SERCHG           PIC S9(5)V99   COMP-3.
           03  TE-EXTCHG           PIC S9(5)V99   COMP-3.

           03  TE-MAINTFEE         PIC S9(5)V99   COMP-3.
           03  TE-ORGTERM          PIC 9(3)       COMP-3.
           03  TE-1STPYAMT         PIC S9(7)V99   COMP-3.
           03  TE-NOREGPY          PIC 9(3)       COMP-3.
           03  TE-REGPYAMT         PIC S9(7)V99   COMP-3.
           03  TE-LASTPYAMT        PIC S9(7)V99   COMP-3.
      *----------------------------------------------------

           03  TE-FEETBL          OCCURS 20 TIMES.
               05  TE-FEECODE      PIC XX.
               05  TE-FEEAMT       PIC S9(5)V99   COMP-3.
               05  TE-FEEOURS      PIC X.
      *----------------------------------------------------
           03  TE-INSCODES.
               05  TE-INSURED      PIC X.
               05  TE-INSTYPES     PIC X.
                   88  TE-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                 "0" "1" "2" "3" "4"
                                                 "5" "6" "7" "8" "9".
                   88  TE-INSTYPES-DL      VALUE "C"
                                                 "G" "I" "J" "M" "P"
                                                 "T" "X" "Y" "5" "6"
                                                 "7".
                   88  TE-INSTYPES-LL      VALUE "B"
                                                 "A" "F" "H" "L" "O"
                                                 "S" "U" "W" "2" "3"
                                                 "4".
                   88  TE-INSTYPES-AH      VALUE "D"
                                                 "F" "G" "K" "L" "M"
                                                 "O" "P" "Q" "V" "W"
                                                 "X" "3" "4" "6" "7"
                                                 "9" "0".
                   88  TE-INSTYPES-PP      VALUE "E"
                                                 "H" "J" "K" "L" "M"
                                                 "U" "Y" "Z" "3" "6"
                                                 "9".
                   88  TE-INSTYPES-PP2     VALUE "R"
                                                 "S" "T" "V" "W" "X"
                                                 "2" "4" "5" "7" "8"
                                                 "0".
                   88  TE-INSTYPES-O1      VALUE "1"
                                                 "A" "I" "O" "P" "Q"
                                                 "U" "Y" "Z" "2" "3"
                                                 "4" "5" "6" "7" "8"
                                                 "9" "0".
               05  TE-INSELIGIBLE  PIC X.
                   88  TE-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  TE-INSELIGIBLE-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  TE-INSELIGIBLE-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  TE-INSELIGIBLE-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  TE-INSELIGIBLE-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  TE-INSELIGIBLE-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  TE-INSELIGIBLE-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
           03  TE-INSTBL          OCCURS 10 TIMES.
               05  TE-INSCOMP      PIC 99.
               05  TE-INSOURS      PIC X.
 
               05  TE-INSEFF-DATE  PIC 9(8)       COMP-3.

               05  TE-INSTERM     REDEFINES TE-INSEFF-DATE
                                   PIC 9(8)       COMP-3.

               05  TE-INSEXP-DATE  PIC 9(8)       COMP-3.
               05  TE-INSPREM      PIC S9(5)V99   COMP-3.

               05  TE-INSCOMM      PIC S9(5)V99   COMP-3.
       


               05  TE-INSANTICERN    PIC S9(7)V99   COMP-3.
               05  TE-INSEARNINGS    PIC S9(7)V99   COMP-3.    
      *----------------------------------------------------
           03  TE-DLNO             PIC 9(4)V99    COMP-3.
           03  TE-DLRECOURSE       PIC X.
           03  TE-DLPARTRCAMT      PIC S9(7)V99   COMP-3.
           03  TE-PURBAL           PIC S9(7)V99   COMP-3.
           03  TE-DLRVPRIN        OCCURS 3
                                   PIC S9(7)V99   COMP-3.

           03  TE-DLRVCHGS        OCCURS 3
                                   PIC S9(7)V99   COMP-3.
           03  TE-DLPARVHELD       PIC S9(7)V99   COMP-3.
           03  TE-DLPARVPAID       PIC S9(7)V99   COMP-3.
           03  TE-DLDISC           PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TE-ANTICTERM        PIC 9(3)       COMP-3.

           03  TE-ANTICERN        OCCURS 10 TIMES
                                   PIC S9(7)V99   COMP-3.

           03  TE-EARNINGS        OCCURS 10 TIMES
                                   PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TE-PRLNNO           PIC 9(8)       COMP-3.
           03  TE-INSPREM-DLADJ    PIC S9(5)V99   COMP-3.
           03  TE-PRDLDISC-UNEARN  PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TE-DL-RVCHGS-CODE  OCCURS 3
                                   PIC X.
           03  TE-DL-PARVHELD-CODE PIC X.
           03  TE-DL-PARVPAID-CODE PIC X.

           03  TE-SPRCLASS         PIC 999        COMP-3.

           03  TE-O3INS-DLPROC     PIC S9(7)V99   COMP-3.
           03  TE-USERID           PIC X(10).

      *>>XDD DATE=YYYYMMDD
           03  TE-POSTDATE         PIC 9(8)       COMP-3.
           03  TE-POSTTIME         PIC 9(6)       COMP-3.
      *NEW
           03  TE-TAX-AMT           PIC S9(7)V99   COMP-3.
           03  TE-APPROVED-BY-USER  PIC X(10).
           03  TE-APPROVED-BY-CODE  PIC X(3).
      *>>XDD DATE=YYYYMMDD
           03  TE-APPROVED-BY-DATE  PIC 9(8)       COMP-3.
           03  TE-APPROVED-BY-TIME  PIC 9(6)       COMP-3.

           03  FILLER               PIC X(135).
