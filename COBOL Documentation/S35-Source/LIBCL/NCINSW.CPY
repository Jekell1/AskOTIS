      * COPYMEMBER: LIBCL/NCINSW
      ******************************************************************
      *    NCINSW
      *    WORKERS FOR NORTH CAROLINA INS. ORIGINATION FEE EDIT:
      *
      * REV:
      * 150310 COMMENT OUT NCINSR-BUF-SSNO, NCINSR-BUF-DUAL-ACCTNO,
      *        SCREEN DISPLAYED JUNK.
      * KEC 2015.0603 REMOVED THE USE OF 'INDEXED BY' ON OCCURS.
      *               MADE THEM BASIC OCCURS FOR USE OF MICROFOCUS.
      * BAH 2017.0405 CHANGED DATES TO 9(8)
      * BAH 2024.0423 REMOVED NCINS-NEW-ACCTNO, NCINS-UPDATE-FG,
      *               NCINS-DUAL-ACCTNO, NCINSR-DUAL-ACCTNO
      ******************************************************************
       01  NCINS-WORKERS.
      * SUBSCRIPTS FOR PREVIOUS INDEXES                 KEC 2015.0603
           03  NCINS-SSSNO               PIC 9(02).
           03  NCINS-FFEE                PIC 9(02).
           03  NCINS-PFEE                PIC 9(02).
           03  NCINS-BFEE                PIC 9(02).
           03  NCINSR-TTBL               PIC 9(02).
           03  NCINSR-IINS               PIC 9(02).
           03  NCINSR-CCNT               PIC 9(02).
           03  NCINS-PATH.
               05  FILLER                PIC X(8)  VALUE "CL/NCINS".
               05  NCINS-NO              PIC X.
      * INPUT:
           03  NCINS-TYPE-CD             PIC X.
           03  NCINS-PRIOR-ACCTNO        PIC 9(8).
      * YYYYMMDD
           03  NCINS-TRANS-DATE          PIC 9(8).
           03  NCINS-ORGST               PIC XX.
           03  NCINS-LN-SSNO             PIC 9(9) OCCURS 4.
           03  NCINS-FEECODE             PIC X(2) OCCURS 3.
           03  NCINS-PD-FEECODE          PIC X(2) OCCURS 10.
      * OUTPUT:
           03  NCINS-BAD-FEECODE-TABLE.
               05  NCINS-BAD-FEECODE     OCCURS 3
                                         PIC X(2).

           03  NCINSR-TABLE.
               05  NCINSR-TBL                    OCCURS 4.
                   07  NCINSR-INS-TABLE          OCCURS 3.
                       09  NCINSR-INS-CNT        OCCURS 2.
                           11  NCINSR-INS-ACCTNO PIC 9(8).
                           11  NCINSR-INS-DATE   PIC 9(8).

           03  NCINSR-BUF.
               05  NCINSR-BUF-TBL                    OCCURS 4.
                   07  NCINSR-BUF-INS-TABLE          OCCURS 3.
                       09  NCINSR-BUF-INS-CNT        OCCURS 2.
                           11  NCINSR-BUF-INS-ACCTNO PIC Z(6).
                           11  NCINSR-BUF-INS-DATE   PIC 99/99/99
                                                     BLANK ZERO.
              05  NCINSR-BUF-FEECODE                 OCCURS 3
                                                     PIC XX.

           03  NCINS-ERRCD               PIC XX.
