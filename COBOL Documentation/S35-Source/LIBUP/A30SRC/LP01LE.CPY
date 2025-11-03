      * COPYMEMBER: LIBLP/LP01LE
      ******************************************************************
      *
      * DESCRIPTION:  LOAN ENDORSER FILE (LE)
      * RECORD SIZE:  150 BYTES
      * RECORD KEY :  LE-ACCTNO
      *               LE-TYPE
      *                  "E" - LOAN ENDORSER RECORDS
      *                  "R" - RESTRICTED RECORDS
      *               LE-FLAG
      *                  " " - NOT USED       [LE-TYPE = "E"]
      *                  "D" - DUAL LOAN      [LE-TYPE = "R"]
      *                  "P" - PAIDOFF LOAN   [LE-TYPE = "R"]
      *                  "R" - RENEWAL        [LE-TYPE = "R"]
      *               LE-SSNO
      *                  NON-ZEROES -         [LE-TYPE = "E"]
      *                  ZEROES -             [LE-TYPE = "R"]
      *               LE-OSACCTNO
      *
      *=================================================================
      * REV:
      * 160509 CS REMOVED REDEFINE OF LE-FLAG, CHANGED LE-FLAG TO
      *           PIC X,
      *            AND LE-SSNO A SEPARATE FIELD. ADD 50 FILLER
      * 160511 CS REMOVED REDEFINE OF RESTRICTED RECORD AND JUST MADE
      *           INCLUSIVE
      *
      * KEC 2017.0324 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ONE BYTE FROM FILLER TO MAKE LE-REC
      *          A SIZE OF 150 BYTES.  LE-SIZE IS NOW 150.
      *
      ******************************************************************
       78  LE-SIZE                     VALUE 150.
       01  LE-REC.
           03  LE-BRNO                 PIC 9(4).
           03  LE1-KEY.
               05  LE-ACCTNO           PIC 9(8).     
               05  LE-TYPE             PIC X.
               05  LE-FLAG             PIC X.
               05  LE-SSNO             PIC 9(9).          
               05  LE-OSACCTNO         PIC 9(8).     

      *>>XDD DATE=YYYYMMDD
           03  LE-LTOUCH-DATE              PIC 9(8)      COMP-3.   

      *=================================================================
      *    LE-TYPE - "E" - LOAN ENDORSER RECORDS
      *    LE-FLAG - " " - NOT USED
      *-----------------------------------------------------------------
      *    LS-OSRATE - 00.0000 (PRINCIPAL ONLY)
      *                ??.???? (EXACT RATE)
      *                99.9999 (NORMAL LOAN RATE)
      *=================================================================
           03  LE-E-OSRATE             PIC 999V999   COMP-3.
           03  LE-E-OSAMT              PIC 9(7)V99   COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LE-E-OSDATE             PIC 9(8)      COMP-3.
           03  LE-E-AMT                PIC 9(7)V99   COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LE-E-DATE               PIC 9(8)      COMP-3.
           03  LE-E-OSMAKERCD          PIC X.

      *=================================================================
      *    LE-TYPE - "R" - RESTRICTED RECORDS
      *    LE-FLAG - "D" - DUAL LOAN
      *              "P" - PAIDOFF LOAN
      *              "R" - RENEWAL
      *=================================================================
      *>>XDD DATE=YYYYMMDD
           03  LE-R-LOANDATE           PIC 9(8)      COMP-3.
           03  LE-R-LNAMT              PIC 9(7)V99   COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  LE-R-POFFDATE           PIC 9(8)      COMP-3.
           03  LE-R-RESTRICTED-MONEY   PIC 9(7)V99   COMP-3.
           03  LE-R-BALANCE            PIC 9(7)V99   COMP-3.
           03  LE-R-SALEFIN            PIC X.

           03  FILLER                  PIC X(63).
