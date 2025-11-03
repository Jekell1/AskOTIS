      * COPYMEMBER: LIBLP/MULSIM
      ******************************************************************
      *                 (MULSIM)
      *  THE FOLLOWING 77 LEVEL ITEM IS USED TO CALL
      *  C LANGUAGE SUBROUTINES.
      * REV:
      *  JTG 020117 **NEW CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99
      *                                           LENDMARK PR#J021
      *  BAH 020625 RENAMED STAT TO MIR-STAT FOR DUPLICATE REASONS
      *
      * KEC 2015.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      *
      ******************************************************************
       77  MIRX                        PIC 9(01). 
       77  MIR-STAT                    PIC XX.
      *                                VALUE "11".

       01  MIR-MANY-INTEREST-RATES-SIMPLE.
           05  MIR-C-VECTOR                    PIC XX
                                               VALUE "11".
           05  FILLER                          PIC XX.
           05  MIR-OUTPUT-VARBS.
               10  MIR-BALANCE                 PIC S9(9) COMP-5.
               10  MIR-PAYMENT                 PIC S9(9) COMP-5.
               10  MIR-BALLOON                 PIC S9(9) COMP-5.
               10  MIR-RETURN-CODE             PIC X.
               10  FILLER                      PIC XXX.
           05  MIR-INPUT-VARBS.
               10  MIR-UPPY                    PIC S9(9) COMP-5.
               10  MIR-TERM                    PIC S9(9) COMP-5.
               10  MIR-INT-CHARGEABLE          PIC S9(9) COMP-5.
               10  MIR-REGPYAMT                PIC S9(9) COMP-5.
               10  MIR-LASTPYAMT            PIC S9(9) COMP-5.
           05  MIR-NO-RATES                    PIC S9(9) COMP-5.
           05  MIR-RATETBL                     OCCURS 5.
      *KEC 2015.0605-------------------------> INDEXED BY MIRX.
               10  MIR-RATE                    PIC S9(9) COMP-5.
               10  MIR-BREAK                   PIC S9(9) COMP-5.
           05  MIR-BALLOON-FG                  PIC X.
      ************************************************************
