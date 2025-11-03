      * COPYMEMBER: LIBLP/LPFDTRX.CPY
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE [CLASS TRANSFERS]
      *
      * FILE INFO  :  PRIMARY LAYOUT IN LIBLP/LPFDTR.CPY
      *               TR1-CODE = X - CLASS TRANSFERS
      *               LOCAL ED/TR###  (####=MMYY)              RSZ=950
      *
      *=================================================================
      * REV:
      * MJD 062795 ADDED TX-USERID, TX-POSTTIME, TX-POSTDATE
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  CS 160915 REMOVED TX-SEQNO
      *            REMOVED  TX-FEETBL,NOT UPDATED 
      *            ADDED TX-ACCTNO 9(8) AND MADE TX-NUMBER REDEFINE
      *            CHANGED  TX-LOANDATE/ TX-POSTDATE FROM 9(6) TO 9(8)
      *            CHANGED TX-MAINTFEE FROM 9(3)V99 TO 9(5)V99 
      *            REMOVED TX-INSTBL OCCURS 10, NOT UPDATED    
      *            CHANGED TX-DLPARTRCAMT 9(6)V99 TO 9(7)V99
      *            CHANGED TX-DLRVCHGS 9(6)V99 TO 9(7)V99
      *            CHANGED TX-DLPARVPAID 9(6)V99 TO 9(7)V99
      *            CHANGED  TX-SPRCLASS 99 TO 999
      *            CHANGED  TX-ANTICERN /TX-EARNINGS OCCURS 6 TO 10  
      *            ADDED TX-INSANTICERN/TX-INSEARNINGS OCCURS 10
      * CS 161117 ADD TX-SUBCLASS
      *  CS 170713 CHANGED TX-LINE TO PIC 9(3) TO MATCH WK2-LINE, IN A15
      *            WAS COMP 9(2) WHICH ALLOWED A VALUE OF 248, I DONT THINK
      *            WE ALLOWED LINE >99 BUT CHANGED TO 999. FILLER TO 4
      *            SAVED LPFDTRX.170713
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      ******************************************************************
       01  TX-REC.
           03  TX-TRX-KEY.
               05  TX-DAY              PIC 99.
               05  TX-BRNO             PIC 9(4).
               05  TX-CODE             PIC X.
               05  TX-CLASS            PIC 999.
               05  TX-ACCTNO           PIC 9(8).
               05  TX-LAWCODE          PIC 99.
               05  TX-LINE             PIC 999.
               05  TX-FILLER           PIC X(4).
      *----------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  TX-LTOUCH-DATE      PIC 9(8) COMP-3.

           03  TX-BORRTBL          OCCURS 4 TIMES.
               05  TX-SSNO         PIC 9(9)       COMP-3.
               05  TX-MAKERCD      PIC X.
           03  TX-PURCD            PIC XXX.
           03  TX-SRCD             PIC XX.
           03  TX-COLLCD           PIC XX.
           03  TX-SERVACCT         PIC X.
           03  TX-LOANTYPE         PIC X.
           03  TX-SUBCLASS         PIC 99.
      *>>XDD DATE=YYYYMMDD
           03  TX-LOANDATE         PIC 9(8)       COMP-3.
           03  TX-PROCEEDS         PIC S9(7)V99   COMP-3.
           03  TX-LNAMT            PIC S9(7)V99   COMP-3.
           03  TX-FINCHG           PIC S9(7)V99   COMP-3.
           03  TX-INTCHG           PIC 9(7)V99    COMP-3.
           03  TX-SERCHG           PIC 9(5)V99    COMP-3.
           03  TX-EXTCHG           PIC 9(5)V99    COMP-3.
           03  TX-MAINTFEE         PIC 9(5)V99     COMP-3.
           03  TX-ORGTERM          PIC 9(3)       COMP-3.
      *----------------------------------------------------
      *% TX-FEETBL OCCURS 10, DID A TRANSFER TO SERVICE WITH FEES AND INSURANCE
      *% NEITHER WERE UPDATED ON THE TX 
      *% MAKES SENSE THAT IT DIDNT OR EOD WOULD HAVE UDPATED GL.
      *  LEFT THESE COMMENTED IN CASE WE HAD TO ADD BACK TO THE RECORD
      *  THEY ARE NOT UPDATED
      *    03  TX-FEETBL           OCCURS 10 TIMES.
      *        05  TX-FEECODE      PIC XX.
      *        05  TX-FEEAMT       PIC S9(5)V99   COMP-3.
      *        05  TX-FEEOURS      PIC X.
      *----------------------------------------------------
           03  TX-INSCODES.
               05  TX-INSURED      PIC X.
      *        05  TX-INSTYPES     PIC X.
      *            88  TX-INSTYPES-VALID   VALUE "A" THRU "Z"
      *                                          "0" "1" "2" "3" "4"
      *                                          "5" "6" "7" "8" "9".
      *            88  TX-INSTYPES-DL      VALUE "C"
      *                                          "G" "I" "J" "M" "P"
      *                                          "T" "X" "Y" "5" "6"
      *                                          "7".
      *            88  TX-INSTYPES-LL      VALUE "B"
      *                                          "A" "F" "H" "L" "O"
      *                                          "S" "U" "W" "2" "3"
      *                                          "4".
      *            88  TX-INSTYPES-AH      VALUE "D"
      *                                          "F" "G" "K" "L" "M"
      *                                          "O" "P" "Q" "V" "W"
      *                                          "X" "3" "4" "6" "7"
      *                                          "9" "0".
      *            88  TX-INSTYPES-PP      VALUE "E"
      *                                          "H" "J" "K" "L" "M"
      *                                          "U" "Y" "Z" "3" "6"
      *                                          "9".
      *            88  TX-INSTYPES-PP2     VALUE "R"
      *                                          "S" "T" "V" "W" "X"
      *                                          "2" "4" "5" "7" "8"
      *                                          "0".
      *            88  TX-INSTYPES-O1      VALUE "1"
      *                                          "A" "I" "O" "P" "Q"
      *                                          "U" "Y" "Z" "2" "3"
      *                                          "4" "5" "6" "7" "8"
      *                                          "9" "0".
      *        05  TX-INSELIGIBLE  PIC X.
      *            88  TX-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
      *                                            "0" "1" "2" "3" "4"
      *                                            "5" "6" "7" "8" "9".
      *            88  TX-INSELIGIBLE-DL     VALUE "C"
      *                                            "G" "I" "J" "M" "P"
      *                                            "T" "X" "Y" "5" "6"
      *                                            "7".
      *            88  TX-INSELIGIBLE-LL     VALUE "B"
      *                                            "A" "F" "H" "L" "O"
      *                                            "S" "U" "W" "2" "3"
      *                                            "4".
      *            88  TX-INSELIGIBLE-AH     VALUE "D"
      *                                            "F" "G" "K" "L" "M"
      *                                            "O" "P" "Q" "V" "W"
      *                                            "X" "3" "4" "6" "7"
      *                                            "9" "0".
      *            88  TX-INSELIGIBLE-PP     VALUE "E"
      *                                            "H" "J" "K" "L" "M"
      *                                            "U" "Y" "Z" "3" "6"
      *                                            "9".
      *            88  TX-INSELIGIBLE-PP2    VALUE "R"
      *                                            "S" "T" "V" "W" "X"
      *                                            "2" "4" "5" "7" "8"
      *                                            "0".
      *            88  TX-INSELIGIBLE-O1     VALUE "1"
      *                                            "A" "I" "O" "P" "Q"
      *                                            "U" "Y" "Z" "2" "3"
      *                                            "4" "5" "6" "7" "8"
      *                                            "9" "0".

      *  LEFT THESE COMMENTED IN CASE WE HAD TO ADD BACK TO THE RECORD
      *  THEY ARE NOT UPDATED
      *    03  TX-INSTBL           OCCURS 10 TIMES.
      *        05  TX-INSCOMP      PIC 99.
      *        05  TX-INSOURS      PIC X.
      *        05  TX-INSEFF       PIC 9(6)       COMP-3.
      *        05  TX-INSTERM      REDEFINES TX-INSEFF
      *                            PIC 9(6)       COMP-3.
      *        05  TX-INSEXP       PIC 9(6)       COMP-3.
      *        05  TX-INSPREM      PIC S9(5)V99   COMP-3.
      *        05  TX-INSCOMM      PIC S9(6)V99   COMP-3.
            
      *%ADDED FOR WHEN WE STORE INSURANCE EARNINGS,A TP FROM OUR
      *CLASS TO SERVICE CLASS GENERATES AND 'X' RECORD FOR OUR CLASS
           03  TX-INSANTICERN  OCCURS 10  PIC S9(7)V99 COMP-3.
           03  TX-INSEARNINGS  OCCURS 10  PIC S9(7)V99 COMP-3.
      *----------------------------------------------------
           03  TX-DLNO             PIC 9(4)V99    COMP-3.
           03  TX-DLRECOURSE       PIC X.
           03  TX-DLPARTRCAMT      PIC 9(7)V99    COMP-3.
           03  TX-PURBAL           PIC S9(7)V99   COMP-3.
           03  TX-DLRVPRIN         OCCURS 3
                                   PIC S9(7)V99   COMP-3.
           03  TX-DLRVCHGS         OCCURS 3
                                   PIC S9(7)V99   COMP-3.
           03  TX-DLPARVHELD       PIC S9(7)V99   COMP-3.
           03  TX-DLPARVPAID       PIC S9(7)V99   COMP-3.
           03  TX-DLDISC           PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TX-ANTICTERM        PIC 9(3)       COMP-3.

           03  TX-ANTICERN         OCCURS 10 TIMES
                                   PIC S9(7)V99   COMP-3.

           03  TX-EARNINGS         OCCURS 10 TIMES
                                   PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TX-PRLNNO           PIC 9(8)       COMP-3.
           03  TX-PRBAL            PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TX-TFRBAL           PIC S9(7)V99   COMP-3.
      *----------------------------------------------------
           03  TX-SPRCLASS         PIC 999        COMP-3.
           03  TX-USERID           PIC X(10).

      *>>XDD DATE=YYYYMMDD
           03  TX-POSTDATE         PIC 9(8)       COMP-3.
           03  TX-POSTTIME         PIC 9(6)       COMP-3.
           03  FILLER              PIC X(545).
