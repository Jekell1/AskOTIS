      * COPYMEMBER: LIBWI/RSVENTW
      **************************************************************************
      *               (RSVENTW)
      *    RESERVE ENTRY WINDOW WORKERS
      *
      * REV:
      * BAH 012596 ADDED RSVENT-LOANTYPE, NEED TO TEST FOR "IB"
      *            LOANS, CANT DO PARTICIPATION
      * JTG 032696 ADDED RSV-HOLDER & RSV-ERR-FG
      * JTG 990318 ADDED LOGIC FOR SP-DLDISC-RVPRIN1-PCNT & SP-LNCOST-FRMAL 'D'
      * JTG 991215 ADDED SP-DLDISC-RVPRIN1-AF-PCNT (AMTFIN), TFC #058
      * JTG 020905 ADDED LN-DLCOMM-03-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
      *            ADDED RSVENT-DLCOMM-PP-CMSI RSVENT-DLCOMM-O3-CMSI
      *            ADDED RSVENT-INSPREM-DLADJ
      **************************************************************************
       01  RSV-ERR-FG                 PIC X.
       01  RSV-HOLDER                 PIC S9(9)V99   COMP.
       01  RSVENT-ERRCD               PIC X.
       78  RSVENT-DLRSV-SEQ                      VALUE 1.

      * NOTE: RSVENTX-FIELDS MUST (MAP) RSVENT-FIELDS:
       01  RSVENT-FIELDS.
           03  RSVENT-ANTICERN           PIC S9(7)V99   COMP.
           03  RSVENT-PROCEEDS           PIC S9(7)V99   COMP.
           03  RSVENT-AMTFIN             PIC S9(7)V99   COMP.
           03  RSVENT-YLDRATE            PIC 9(3)V9(3)  COMP.
           03  RSVENT-DLRVPRIN           OCCURS 3
                                         PIC S9(7)V99   COMP.
           03  RSVENT-DLRVCHGS           OCCURS 3
                                         PIC S9(7)V99   COMP.
           03  RSVENT-DLPARVHELD         PIC S9(7)V99   COMP.
           03  RSVENT-DLPARVPAID         PIC S9(7)V99   COMP.
           03  RSVENT-DLDISC             PIC S9(7)V99   COMP.

           03  RSVENT-DLRVPRIN-FG        OCCURS 3
                                         PIC X.
           03  RSVENT-DLRVCHGS-FG        OCCURS 3
                                         PIC X.
           03  RSVENT-DLPARVHELD-FG      PIC X.
           03  RSVENT-DLPARVPAID-FG      PIC X.
           03  RSVENT-DLDISC-FG          PIC X.
           03  RSVENT-LOANTYPE           PIC X.
           03  RSVENT-LNCOST             PIC S9(7)V99   COMP.
           03  RSVENT-LNCOST-FG          PIC X.
           03  RSVENT-INSPREM-DLADJ      PIC S9(7)V99   COMP.
           03  RSVENT-DLCOMM-PP-CMSI-FG  PIC X.
           03  RSVENT-DLCOMM-PP-CMSI     PIC S9(7)V99   COMP.
           03  RSVENT-DLCOMM-O3-CMSI-FG  PIC X.
           03  RSVENT-DLCOMM-O3-CMSI     PIC S9(7)V99   COMP.

      ********************************************************
      *    RESERVE ENTRY AMOUNT WORKERS
      ********************************************************
       01  RSVENTX-FIELDS.
           03  RSVENTX-ANTICERN           PIC S9(7)V99  COMP.
           03  RSVENTX-PROCEEDS           PIC S9(7)V99  COMP.
           03  RSVENTX-AMTFIN             PIC S9(7)V99  COMP.
           03  RSVENTX-YLDRATE            PIC 9(3)V9(3) COMP.
           03  RSVENTX-DLRVPRIN           OCCURS 3
                                          PIC S9(7)V99  COMP.
           03  RSVENTX-DLRVCHGS           OCCURS 3
                                          PIC S9(7)V99  COMP.
           03  RSVENTX-DLPARVHELD         PIC S9(7)V99  COMP.
           03  RSVENTX-DLPARVPAID         PIC S9(7)V99  COMP.
           03  RSVENTX-DLDISC             PIC S9(7)V99  COMP.

           03  RSVENTX-DLRVPRIN-FG        OCCURS 3
                                          PIC X.
           03  RSVENTX-DLRVCHGS-FG        OCCURS 3
                                          PIC X.
           03  RSVENTX-DLPARVHELD-FG      PIC X.
           03  RSVENTX-DLPARVPAID-FG      PIC X.
           03  RSVENTX-DLDISC-FG          PIC X.
           03  RSVENTX-LOANTYPE           PIC X.
           03  RSVENTX-LNCOST             PIC S9(7)V99  COMP.
           03  RSVENTX-LNCOST-FG          PIC X.
           03  RSVENTX-INSPREM-DLADJ      PIC S9(7)V99   COMP.
           03  RSVENTX-DLCOMM-PP-CMSI-FG  PIC X.
           03  RSVENTX-DLCOMM-PP-CMSI     PIC S9(7)V99   COMP.
           03  RSVENTX-DLCOMM-O3-CMSI-FG  PIC X.
           03  RSVENTX-DLCOMM-O3-CMSI     PIC S9(7)V99   COMP.

      ********************************************************
      *   RESERVE ENTRY SCREEN DISPLAY BUFFER
      ********************************************************
       01  RSV-DISPLAY-BUF    VALUE SPACES.
           03  RSVANTIC       PIC ZZZZZ9.99-.
           03  RSVPROC        PIC ZZZZZ9.99-.
           03  RSVYIELD       PIC ZZ9.999.
           03  RSV01          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV011         PIC X.
           03  RSV02          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV021         PIC X.
           03  RSV03          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV031         PIC X.
           03  RSV04          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV041         PIC X.
           03  RSV05          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV051         PIC X.
           03  RSV06          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV061         PIC X.
           03  RSV07          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV071         PIC X.
           03  RSV08          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV081         PIC X.
           03  RSV09          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV091         PIC X.
           03  RSV10          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV101         PIC X.
           03  RSV11          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV111         PIC X.
           03  RSV12          PIC ZZZZZ9.99   BLANK ZERO.
           03  RSV121         PIC X.

       01  RSV-DISPLAY-LIST.
           03  FILLER         PIC X(17) VALUE
           "ANTIC PROC YIELD ".
           03  FILLER         PIC X(36) VALUE
           "010 011 020 021 030 031 040 041 050 ".
           03  FILLER         PIC X(36) VALUE
           "051 060 061 070 071 080 081 090 091 ".
           03  FILLER         PIC X(24) VALUE
           "100 101 110 111 120 121.".

       01  RSV-DISPLAY-BUF-T  VALUE SPACES.
           03  RSVANTIC-T     PIC ZZZZZ9.99-.
           03  RSVPROC-T      PIC ZZZZZ9.99-.
           03  RSVYIELD-T     PIC ZZ9.999.
       01  RSV-DISPLAY-LIST-T.
           03  FILLER         PIC X(20) VALUE
           "TANTIC TPROC TYIELD.".

       01  RSVMAX             PIC 99   VALUE 12.
       01  RSV-ONE            PIC S9   COMP.
       01  RSV-ELE            PIC 9(2) COMP.

      *    **********************************
      *    *   RSV-ELEMENT SPECIFICATIONS
      *    **********************************
       01  RSVSPEC-TABLE.
      * 1. RESERVE PRINCIPAL #1
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      * 2. RESERVE PRINCIPAL #2
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      * 3. RESERVE PRINCIPAL #3
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      * 4. RESERVE CHARGES #1
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      * 5. RESERVE CHARGES #2
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      * 6. RESERVE CHARGES #3
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      * 7. PARTICIPATING RESERVE HELD
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      * 8. PARTICIPATING RESERVE PAID
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      * 9. DEALER DISCOUNT
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      *10. LOAN COSTS
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      *11. DLCOMM PP CMSI
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
      *12. DLCOMM O3 CMSI
           03  FILLER         PIC X(8) VALUE "ENNN062 ".
       01  RSVSPEC-TAB        REDEFINES RSVSPEC-TABLE.
           03  RSVSPEC-REC    OCCURS 12 TIMES.
               05  RSVSPEC    PIC X(7).
               05  RSVCHFG    PIC X.
