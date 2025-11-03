      * COPYMEMBER: LIBFP/PST.INFOW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/PST.INFOW
      *
      * DESCRIPTION   :  LASER (PST) FORM INFO WORKERS
      *
      *=================================================================
      * REV:
      * KEC 2007.1101 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ***NEW*** PST
      *
      * KEC 2009.0306 {PR#03333} MERIT MILITARY <A15> [DAVID SINCLAIR]
      *               ADDED USE OF PAYOFF-AMT-APP-LATECHG.
      *
      * KEC 2009.0611 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED BORROWERS PRINT LAYOUT (PBL).
      *
      * KEC 2011.0930 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ***NEW*** POSTSCRIPT
      * BAH 2016.0114 CHANGED AUTO AND HHGS TO MATCH LTCFILE, PD0003
      *
      * KEC 2017.0224 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED SIZE OF INS-MAXCOVR (SP-INSMAXCHGABL) AND
      *          MONTHLY-DISABILITY-BENEFIT (SP-INSMAXBENIF)
      *          TO PIC 9(7)V99.
      *
      * KEC 2018.0920 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED FIELD SIZES TO MATCH BW FIELD SIZES.
      * AMM 2021.0302 {PR#00000} PARADATA <A32> [JAY CISZEWSKI]
      *          REMOVED PRIOR-ACCTNO-SEQNO
      *
      ******************************************************************
      *
      *    P I B W - W O R K E R S
      *
      *=================================================================
      *    DESC: PST-INFO-BORROWERS-WORKERS (PIBW)
      ******************************************************************
       78  PIBW-MAKER                                 VALUE 1.
       78  PIBW-JOINT                                 VALUE 2.
       78  PIBW-ENDR1                                 VALUE 3.
       78  PIBW-ENDR2                                 VALUE 4.
       78  PIBW-ENDR3                                 VALUE 5.
       78  PIBW-SPOUS                                 VALUE 6.
       78  PIBW-MAX                                   VALUE 6.
       77  PIBW-ELE                     PIC 9(01)     VALUE ZEROES.
       77  PIBW-SUB                     PIC 9(02)     VALUE ZEROES.
       77  PIBW-SUB-JOINT               PIC 9(01)     VALUE ZEROES.
       77  PIBW-SUB-ENDR1               PIC 9(01)     VALUE ZEROES.
       77  PIBW-SUB-ENDR2               PIC 9(01)     VALUE ZEROES.
       77  PIBW-SUB-ENDR3               PIC 9(01)     VALUE ZEROES.

      *=================================================================
       01  PIBW-WORKERS.
           03  PIBW-TABLE                          OCCURS PIBW-MAX.
               05  PIBW-ADDRESS-1                   PIC X(30).
               05  PIBW-ADDRESS-2                   PIC X(30).
               05  PIBW-AGE                         PIC 9(06).
               05  PIBW-BDATE                       PIC 9(08).
               05  PIBW-CITY                        PIC X(30).

               05  PIBW-CODE                        PIC X(01).
                   88  PIBW-CODE-M                      VALUE "M".
                   88  PIBW-CODE-J                      VALUE "J".
                   88  PIBW-CODE-E                      VALUE "E".

               05  PIBW-COUNTY                      PIC X(24).
               05  PIBW-DEPENDENTS                  PIC 9(02).

               05  PIBW-EMPLOYMENT.
                   07  PIBW-EMPLOYER                PIC X(35).
                   07  PIBW-POSITION                PIC X(24).
                   07  PIBW-WORK-ADDR1              PIC X(30).
                   07  PIBW-WORK-ADDR2              PIC X(30).
                   07  PIBW-WORKPHONE               PIC 9(10).
                   07  PIBW-WORKEXT                 PIC 9(04).
                   07  PIBW-WORK-YEARS              PIC 9(02).

               05  PIBW-FNAME                       PIC X(35).
               05  PIBW-HOMEPHONE                   PIC 9(10).
               05  PIBW-INCOME                      PIC 9(06)V99.
               05  PIBW-LNAME                       PIC X(35).
               05  PIBW-OTHER-INCOME                PIC 9(06)V99.
               05  PIBW-OTHER-INCOME-SOURCE         PIC X(08).

               05  PIBW-SSNO                        PIC 9(09).
               05  PIBW-STATE                       PIC X(02).

               05  PIBW-STRING-SETUP                VALUE SPACES.
                   07  PIBW-ADDRESS                 PIC X(80).
                   07  PIBW-ADDRESS-CSZ             PIC X(80).
                   07  PIBW-CSZ                     PIC X(80).
                   07  PIBW-NAME-ADDRESS-CSZ        PIC X(80).
                   07  PIBW-NAME-FIRST-LAST         PIC X(80).
                   07  PIBW-NAME-LAST-FIRST         PIC X(80).

               05  PIBW-ZIP                         PIC X(10).

      *-----------------------------------------------------------------
       01  PIBW-ELEMENT              REDEFINES PIBW-WORKERS.

      *=================================================================
      *    M A K E R - E L E M E N T S
      *=================================================================
           03  MAKER-ADDRESS-1                     PIC X(30).
           03  MAKER-ADDRESS-2                     PIC X(30).
           03  MAKER-AGE                           PIC 9(06).

           03  MAKER-BDATE                         PIC 9(08).
           03  MAKER-BDATE-RDF           REDEFINES MAKER-BDATE.
               05  MAKER-BDATE-CC                  PIC 9(02).
               05  MAKER-BDATE-YY                  PIC 9(02).
               05  MAKER-BDATE-MM                  PIC 9(02).
               05  MAKER-BDATE-DD                  PIC 9(02).

           03  MAKER-CITY                          PIC X(30).

           03  MAKER-CODE                          PIC X(01).
               88  MAKER-CODE-M                        VALUE "M".
               88  MAKER-CODE-J                        VALUE "J".
               88  MAKER-CODE-E                        VALUE "E".

           03  MAKER-COUNTY                        PIC X(24).
           03  MAKER-DEPENDENTS                    PIC 9(02).

           03  MAKER-EMPLOYMENT.
               05  MAKER-EMPLOYER                  PIC X(35).
               05  MAKER-POSITION                  PIC X(24).
               05  MAKER-WORK-ADDR1                PIC X(30).
               05  MAKER-WORK-ADDR2                PIC X(30).

               05  MAKER-WORKPHONE                 PIC 9(10).
               05  MAKER-WORKPHONE-RDF   REDEFINES MAKER-WORKPHONE.
                   07  MAKER-WORKPHONE-AREA        PIC 9(03).
                   07  MAKER-WORKPHONE-3           PIC 9(03).
                   07  MAKER-WORKPHONE-4           PIC 9(04).

               05  MAKER-WORKEXT                   PIC 9(04).
               05  MAKER-WORK-YEARS                PIC 9(02).

           03  MAKER-FNAME                         PIC X(35).
           03  MAKER-HOMEPHONE                     PIC 9(10).
           03  MAKER-INCOME                        PIC 9(06)V99.
           03  MAKER-LNAME                         PIC X(35).
           03  MAKER-OTHER-INCOME                  PIC 9(06)V99.
           03  MAKER-OTHER-INCOME-SOURCE           PIC X(08).

           03  MAKER-SSNO                          PIC 9(09).
           03  MAKER-SSNO-RDF            REDEFINES MAKER-SSNO.
               05  MAKER-SSNO-3                    PIC 9(03).
               05  MAKER-SSNO-2                    PIC 9(02).
               05  MAKER-SSNO-4                    PIC 9(04).

           03  MAKER-STATE                         PIC X(02).

           03  MAKER-STRING-SETUP.
               05  MAKER-ADDRESS                   PIC X(80).
               05  MAKER-ADDRESS-CSZ               PIC X(80).
               05  MAKER-CSZ                       PIC X(80).
               05  MAKER-NAME-ADDRESS-CSZ          PIC X(80).
               05  MAKER-NAME-FIRST-LAST           PIC X(80).
               05  MAKER-NAME-LAST-FIRST           PIC X(80).

           03  MAKER-ZIP                           PIC X(10).

      *=================================================================
      *    J O I N T - E L E M E N T S
      *=================================================================
           03  JOINT-ADDRESS-1                     PIC X(30).
           03  JOINT-ADDRESS-2                     PIC X(30).
           03  JOINT-AGE                           PIC 9(06).

           03  JOINT-BDATE                         PIC 9(08).
           03  JOINT-BDATE-RDF           REDEFINES JOINT-BDATE.
               05  JOINT-BDATE-CC                  PIC 9(02).
               05  JOINT-BDATE-YY                  PIC 9(02).
               05  JOINT-BDATE-MM                  PIC 9(02).
               05  JOINT-BDATE-DD                  PIC 9(02).

           03  JOINT-CITY                          PIC X(30).

           03  JOINT-CODE                          PIC X(01).
               88  JOINT-CODE-M                        VALUE "M".
               88  JOINT-CODE-J                        VALUE "J".
               88  JOINT-CODE-E                        VALUE "E".

           03  JOINT-COUNTY                        PIC X(24).
           03  JOINT-DEPENDENTS                    PIC 9(02).

           03  JOINT-EMPLOYMENT.
               05  JOINT-EMPLOYER                  PIC X(35).
               05  JOINT-POSITION                  PIC X(24).
               05  JOINT-WORK-ADDR1                PIC X(30).
               05  JOINT-WORK-ADDR2                PIC X(30).

               05  JOINT-WORKPHONE                 PIC 9(10).
               05  JOINT-WORKPHONE-RDF   REDEFINES JOINT-WORKPHONE.
                   07  JOINT-WORKPHONE-AREA        PIC 9(03).
                   07  JOINT-WORKPHONE-3           PIC 9(03).
                   07  JOINT-WORKPHONE-4           PIC 9(04).

               05  JOINT-WORKEXT                   PIC 9(04).
               05  JOINT-WORK-YEARS                PIC 9(02).

           03  JOINT-FNAME                         PIC X(35).
           03  JOINT-HOMEPHONE                     PIC 9(10).
           03  JOINT-INCOME                        PIC 9(06)V99.
           03  JOINT-LNAME                         PIC X(35).
           03  JOINT-OTHER-INCOME                  PIC 9(06)V99.
           03  JOINT-OTHER-INCOME-SOURCE           PIC X(08).

           03  JOINT-SSNO                          PIC 9(09).
           03  JOINT-SSNO-RDF            REDEFINES JOINT-SSNO.
               05  JOINT-SSNO-3                    PIC 9(03).
               05  JOINT-SSNO-2                    PIC 9(02).
               05  JOINT-SSNO-4                    PIC 9(04).

           03  JOINT-STATE                         PIC X(02).

           03  JOINT-STRING-SETUP.
               05  JOINT-ADDRESS                   PIC X(80).
               05  JOINT-ADDRESS-CSZ               PIC X(80).
               05  JOINT-CSZ                       PIC X(80).
               05  JOINT-NAME-ADDRESS-CSZ          PIC X(80).
               05  JOINT-NAME-FIRST-LAST           PIC X(80).
               05  JOINT-NAME-LAST-FIRST           PIC X(80).

           03  JOINT-ZIP                           PIC X(10).

      *=================================================================
      *         E N D O R S E R 1 - E L E M E N T S
      *=================================================================
           03  ENDR1-ADDRESS-1                     PIC X(30).
           03  ENDR1-ADDRESS-2                     PIC X(30).
           03  ENDR1-AGE                           PIC 9(06).

           03  ENDR1-BDATE                         PIC 9(08).
           03  ENDR1-BDATE-RDF           REDEFINES ENDR1-BDATE.
               05  ENDR1-BDATE-CC                  PIC 9(02).
               05  ENDR1-BDATE-YY                  PIC 9(02).
               05  ENDR1-BDATE-MM                  PIC 9(02).
               05  ENDR1-BDATE-DD                  PIC 9(02).

           03  ENDR1-CITY                          PIC X(30).

           03  ENDR1-CODE                          PIC X(01).
               88  ENDR1-CODE-M                        VALUE "M".
               88  ENDR1-CODE-J                        VALUE "J".
               88  ENDR1-CODE-E                        VALUE "E".

           03  ENDR1-COUNTY                        PIC X(24).
           03  ENDR1-DEPENDENTS                    PIC 9(02).

           03  ENDR1-EMPLOYMENT.
               05  ENDR1-EMPLOYER                  PIC X(35).
               05  ENDR1-POSITION                  PIC X(24).
               05  ENDR1-WORK-ADDR1                PIC X(30).
               05  ENDR1-WORK-ADDR2                PIC X(30).

               05  ENDR1-WORKPHONE                 PIC 9(10).
               05  ENDR1-WORKPHONE-RDF   REDEFINES ENDR1-WORKPHONE.
                   07  ENDR1-WORKPHONE-AREA        PIC 9(03).
                   07  ENDR1-WORKPHONE-3           PIC 9(03).
                   07  ENDR1-WORKPHONE-4           PIC 9(04).

               05  ENDR1-WORKEXT                   PIC 9(04).
               05  ENDR1-WORK-YEARS                PIC 9(02).

           03  ENDR1-FNAME                         PIC X(35).
           03  ENDR1-HOMEPHONE                     PIC 9(10).
           03  ENDR1-INCOME                        PIC 9(06)V99.
           03  ENDR1-LNAME                         PIC X(35).
           03  ENDR1-OTHER-INCOME                  PIC 9(06)V99.
           03  ENDR1-OTHER-INCOME-SOURCE           PIC X(08).

           03  ENDR1-SSNO                          PIC 9(09).
           03  ENDR1-SSNO-RDF            REDEFINES ENDR1-SSNO.
               05  ENDR1-SSNO-3                    PIC 9(03).
               05  ENDR1-SSNO-2                    PIC 9(02).
               05  ENDR1-SSNO-4                    PIC 9(04).

           03  ENDR1-STATE                         PIC X(02).

           03  ENDR1-STRING-SETUP.
               05  ENDR1-ADDRESS                   PIC X(80).
               05  ENDR1-ADDRESS-CSZ               PIC X(80).
               05  ENDR1-CSZ                       PIC X(80).
               05  ENDR1-NAME-ADDRESS-CSZ          PIC X(80).
               05  ENDR1-NAME-FIRST-LAST           PIC X(80).
               05  ENDR1-NAME-LAST-FIRST           PIC X(80).

           03  ENDR1-ZIP                           PIC X(10).

      *=================================================================
      *         E N D O R S E R 2 - E L E M E N T S
      *=================================================================
           03  ENDR2-ADDRESS-1                     PIC X(30).
           03  ENDR2-ADDRESS-2                     PIC X(30).
           03  ENDR2-AGE                           PIC 9(06).

           03  ENDR2-BDATE                         PIC 9(08).
           03  ENDR2-BDATE-RDF           REDEFINES ENDR2-BDATE.
               05  ENDR2-BDATE-CC                  PIC 9(02).
               05  ENDR2-BDATE-YY                  PIC 9(02).
               05  ENDR2-BDATE-MM                  PIC 9(02).
               05  ENDR2-BDATE-DD                  PIC 9(02).

           03  ENDR2-CITY                          PIC X(30).

           03  ENDR2-CODE                          PIC X(01).
               88  ENDR2-CODE-M                        VALUE "M".
               88  ENDR2-CODE-J                        VALUE "J".
               88  ENDR2-CODE-E                        VALUE "E".

           03  ENDR2-COUNTY                        PIC X(24).
           03  ENDR2-DEPENDENTS                    PIC 9(02).

           03  ENDR2-EMPLOYMENT.
               05  ENDR2-EMPLOYER                  PIC X(35).
               05  ENDR2-POSITION                  PIC X(24).
               05  ENDR2-WORK-ADDR1                PIC X(30).
               05  ENDR2-WORK-ADDR2                PIC X(30).

               05  ENDR2-WORKPHONE                 PIC 9(10).
               05  ENDR2-WORKPHONE-RDF   REDEFINES ENDR2-WORKPHONE.
                   07  ENDR2-WORKPHONE-AREA        PIC 9(03).
                   07  ENDR2-WORKPHONE-3           PIC 9(03).
                   07  ENDR2-WORKPHONE-4           PIC 9(04).

               05  ENDR2-WORKEXT                   PIC 9(04).
               05  ENDR2-WORK-YEARS                PIC 9(02).

           03  ENDR2-FNAME                         PIC X(35).
           03  ENDR2-HOMEPHONE                     PIC 9(10).
           03  ENDR2-INCOME                        PIC 9(06)V99.
           03  ENDR2-LNAME                         PIC X(35).
           03  ENDR2-OTHER-INCOME                  PIC 9(06)V99.
           03  ENDR2-OTHER-INCOME-SOURCE           PIC X(08).

           03  ENDR2-SSNO                          PIC 9(09).
           03  ENDR2-SSNO-RDF            REDEFINES ENDR2-SSNO.
               05  ENDR2-SSNO-3                    PIC 9(03).
               05  ENDR2-SSNO-2                    PIC 9(02).
               05  ENDR2-SSNO-4                    PIC 9(04).

           03  ENDR2-STATE                         PIC X(02).

           03  ENDR2-STRING-SETUP.
               05  ENDR2-ADDRESS                   PIC X(80).
               05  ENDR2-ADDRESS-CSZ               PIC X(80).
               05  ENDR2-CSZ                       PIC X(80).
               05  ENDR2-NAME-ADDRESS-CSZ          PIC X(80).
               05  ENDR2-NAME-FIRST-LAST           PIC X(80).
               05  ENDR2-NAME-LAST-FIRST           PIC X(80).

           03  ENDR2-ZIP                           PIC X(10).

      *=================================================================
      *         E N D O R S E R 3 - E L E M E N T S
      *=================================================================
           03  ENDR3-ADDRESS-1                     PIC X(30).
           03  ENDR3-ADDRESS-2                     PIC X(30).
           03  ENDR3-AGE                           PIC 9(06).

           03  ENDR3-BDATE                         PIC 9(08).
           03  ENDR3-BDATE-RDF           REDEFINES ENDR3-BDATE.
               05  ENDR3-BDATE-CC                  PIC 9(02).
               05  ENDR3-BDATE-YY                  PIC 9(02).
               05  ENDR3-BDATE-MM                  PIC 9(02).
               05  ENDR3-BDATE-DD                  PIC 9(02).

           03  ENDR3-CITY                          PIC X(30).

           03  ENDR3-CODE                          PIC X(01).
               88  ENDR3-CODE-M                        VALUE "M".
               88  ENDR3-CODE-J                        VALUE "J".
               88  ENDR3-CODE-E                        VALUE "E".

           03  ENDR3-COUNTY                        PIC X(24).
           03  ENDR3-DEPENDENTS                    PIC 9(02).

           03  ENDR3-EMPLOYMENT.
               05  ENDR3-EMPLOYER                  PIC X(35).
               05  ENDR3-POSITION                  PIC X(24).
               05  ENDR3-WORK-ADDR1                PIC X(30).
               05  ENDR3-WORK-ADDR2                PIC X(30).

               05  ENDR3-WORKPHONE                 PIC 9(10).
               05  ENDR3-WORKPHONE-RDF   REDEFINES ENDR3-WORKPHONE.
                   07  ENDR3-WORKPHONE-AREA        PIC 9(03).
                   07  ENDR3-WORKPHONE-3           PIC 9(03).
                   07  ENDR3-WORKPHONE-4           PIC 9(04).

               05  ENDR3-WORKEXT                   PIC 9(04).
               05  ENDR3-WORK-YEARS                PIC 9(02).

           03  ENDR3-FNAME                         PIC X(35).
           03  ENDR3-HOMEPHONE                     PIC 9(10).
           03  ENDR3-INCOME                        PIC 9(06)V99.
           03  ENDR3-LNAME                         PIC X(35).
           03  ENDR3-OTHER-INCOME                  PIC 9(06)V99.
           03  ENDR3-OTHER-INCOME-SOURCE           PIC X(08).

           03  ENDR3-SSNO                          PIC 9(09).
           03  ENDR3-SSNO-RDF            REDEFINES ENDR3-SSNO.
               05  ENDR3-SSNO-3                    PIC 9(03).
               05  ENDR3-SSNO-2                    PIC 9(02).
               05  ENDR3-SSNO-4                    PIC 9(04).

           03  ENDR3-STATE                         PIC X(02).

           03  ENDR3-STRING-SETUP.
               05  ENDR3-ADDRESS                   PIC X(80).
               05  ENDR3-ADDRESS-CSZ               PIC X(80).
               05  ENDR3-CSZ                       PIC X(80).
               05  ENDR3-NAME-ADDRESS-CSZ          PIC X(80).
               05  ENDR3-NAME-FIRST-LAST           PIC X(80).
               05  ENDR3-NAME-LAST-FIRST           PIC X(80).

           03  ENDR3-ZIP                           PIC X(10).

      *=================================================================
      *         S P O U S E - E L E M E N T S
      *=================================================================
           03  SPOUS-ADDRESS-1                     PIC X(30).
           03  SPOUS-ADDRESS-2                     PIC X(30).
           03  SPOUS-AGE                           PIC 9(06).

           03  SPOUS-BDATE                         PIC 9(08).
           03  SPOUS-BDATE-RDF           REDEFINES SPOUS-BDATE.
               05  SPOUS-BDATE-CC                  PIC 9(02).
               05  SPOUS-BDATE-YY                  PIC 9(02).
               05  SPOUS-BDATE-MM                  PIC 9(02).
               05  SPOUS-BDATE-DD                  PIC 9(02).

           03  SPOUS-CITY                          PIC X(30).

           03  SPOUS-CODE                          PIC X(01).
               88  SPOUS-CODE-M                        VALUE "M".
               88  SPOUS-CODE-J                        VALUE "J".
               88  SPOUS-CODE-E                        VALUE "E".

           03  SPOUS-COUNTY                        PIC X(24).
           03  SPOUS-DEPENDENTS                    PIC 9(02).

           03  SPOUS-EMPLOYMENT.
               05  SPOUS-EMPLOYER                  PIC X(35).
               05  SPOUS-POSITION                  PIC X(24).
               05  SPOUS-WORK-ADDR1                PIC X(30).
               05  SPOUS-WORK-ADDR2                PIC X(30).

               05  SPOUS-WORKPHONE                 PIC 9(10).
               05  SPOUS-WORKPHONE-RDF   REDEFINES SPOUS-WORKPHONE.
                   07  SPOUS-WORKPHONE-AREA        PIC 9(03).
                   07  SPOUS-WORKPHONE-3           PIC 9(03).
                   07  SPOUS-WORKPHONE-4           PIC 9(04).

               05  SPOUS-WORKEXT                   PIC 9(04).
               05  SPOUS-WORK-YEARS                PIC 9(02).

           03  SPOUS-FNAME                         PIC X(35).
           03  SPOUS-HOMEPHONE                     PIC 9(10).
           03  SPOUS-INCOME                        PIC 9(06)V99.
           03  SPOUS-LNAME                         PIC X(35).
           03  SPOUS-OTHER-INCOME                  PIC 9(06)V99.
           03  SPOUS-OTHER-INCOME-SOURCE           PIC X(08).

           03  SPOUS-SSNO                          PIC 9(09).
           03  SPOUS-SSNO-RDF            REDEFINES SPOUS-SSNO.
               05  SPOUS-SSNO-3                    PIC 9(03).
               05  SPOUS-SSNO-2                    PIC 9(02).
               05  SPOUS-SSNO-4                    PIC 9(04).

           03  SPOUS-STATE                         PIC X(02).

           03  SPOUS-STRING-SETUP.
               05  SPOUS-ADDRESS                   PIC X(80).
               05  SPOUS-ADDRESS-CSZ               PIC X(80).
               05  SPOUS-CSZ                       PIC X(80).
               05  SPOUS-NAME-ADDRESS-CSZ          PIC X(80).
               05  SPOUS-NAME-FIRST-LAST           PIC X(80).
               05  SPOUS-NAME-LAST-FIRST           PIC X(80).

           03  SPOUS-ZIP                           PIC X(10).

      ******************************************************************
      *
      *    P S T - I N F O - B P L - W O R K E R S
      *
      *=================================================================
      *    DESC: BORROWERS PRINT LAYOUT (PBL)
      *          USED TO PRINT BORROWER INFO IN A SPECFIC WAY.
      *
      *    IN  : BPL-MAX = 1         MAKER NAME
      *                              JOINT NAME
      *                              MAKER ADDRESS
      *                              MAKER CSZ
      *
      *    IN  : BPL-MAX = 2         MAKER NAME
      *                              JOINT/ENDR1 NAME
      *                              MAKER ADDRESS
      *                              MAKER CSZ
      *
      *    IN  : BPL-MAX = 3         MAKER NAME
      *                              JOINT/ENDR1 NAME
      *                              ENDR1/ENDR2 NAME
      *                              MAKER ADDRESS
      *                              MAKER CSZ
      *
      *    IN  : BPL-MAX = 4         MAKER NAME
      *                              JOINT/ENDR1 NAME
      *                              ENDR1/ENDR2 NAME
      *                              ENDR2/ENDR3 NAME
      *                              MAKER ADDRESS
      *                              MAKER CSZ
      *
      ******************************************************************
       78  BPL---MAX                                 VALUE 06.

      *-----------------------------------------------------------------
       77  BPL-ELE                     PIC 9(02)     VALUE ZEROES.
           88  BPL-ELE-GOOD                          VALUE 04.

       77  BPL-MAX                     PIC 9(02)     VALUE ZEROES.
       77  BPL-STOP                    PIC 9(02)     VALUE ZEROES.

      *=================================================================
       01  BPL-TABLE.
           03  BPL-INFO                PIC X(40) OCCURS BPL---MAX TIMES.
      ******************************************************************
      *
      *    P S T - I N F O - B R A N C H - W O R K E R S
      *
      ******************************************************************
       01  BRANCH-INFORMATION-WORKERS.
           03  BRANCH-ADDRESS          PIC X(50).
           03  BRANCH-CSZ              PIC X(30).
           03  BRANCH-NAME             PIC X(40).

      ******************************************************************
      *
      *    P S T - I N F O - C H E C K - W O R K E R S
      *
      ******************************************************************
       78  CHECK-MAX                                 VALUE 30.
       77  CHECK-SUB                   PIC 9(02)     VALUE ZEROES.
       77  CHECK-MAX-CUSTOMIZE         PIC 9(02)     VALUE ZEROES.

      *-----------------------------------------------------------------
       01  CHECK-INFORMATION-TABLE                   OCCURS CHECK-MAX.
           03  CHECK-AMT               PIC S9(7)V99  COMP-3.
           03  CHECK-NUM               PIC 9(06)     COMP-3.
           03  CHECK-TXT               PIC X(30).
           03  CHECK-PAYEE             REDEFINES CHECK-TXT
                                       PIC X(30).

      *-----------------------------------------------------------------
       01  CHECK-AMT-REMAINING         PIC S9(7)V99  COMP-3.
       01  CHECK-AMT-TO-BORROWER       PIC S9(7)V99  COMP-3.

      ******************************************************************
      *
      *    P S T - I N F O - C O D E - W O R K E R S
      *
      ******************************************************************
       01  PST-INFO-CODE-WORKERS.
           03  CODE-DESC-OF-COLLCD     PIC X(35)     VALUE SPACES.
           03  CODE-DESC-OF-COLLATERAL REDEFINES CODE-DESC-OF-COLLCD
                                       PIC X(35).

      ******************************************************************
      *
      *    P S T - I N F O - C O L L A T E R A L - W O R K E R S
      *
      *-----------------------------------------------------------------
      *    AUTO = AUTOMOBLIE      (1 & 2)
      *    HHGS = HOUSEHOLD GOODS (1 & 2)
      ******************************************************************
       78  AUTO-MAX                                  VALUE 03.
       78  HHGS-MAX                                  VALUE 03.
       77  AUTO-SUB                    PIC 9(01)     VALUE ZEROES.
       77  HHGS-SUB                    PIC 9(01)     VALUE ZEROES.

      *-----------------------------------------------------------------
       01  AUTO-INFORMATION-TABLE.
           03  AUTO-INFO               PIC X(163)    OCCURS AUTO-MAX.

       01  AUTO-INFORMATION-TABLE-RDF  REDEFINES AUTO-INFORMATION-TABLE.
           03  AUTO-INFO-TBL                         OCCURS AUTO-MAX.
               05  AUTO-DESCRIPTION    PIC X(15).
               05  AUTO-MAKE           PIC X(15).
               05  AUTO-MODEL          PIC X(15).
               05  AUTO-YEAR           PIC X(04).
               05  AUTO-STYLE          PIC X(08).
               05  AUTO-CLASS          PIC X(08).
               05  AUTO-ID             PIC X(20).
               05  AUTO-TITLE-NO       PIC X(20).
               05  AUTO-MILEAGE        PIC S9(7)     COMP-3.
               05  AUTO-VALUE          PIC S9(7)V99  COMP-3.
               05  AUTO-SELLPR         PIC S9(7)V99  COMP-3.
               05  AUTO-MISC-AMT       PIC S9(7)V99  COMP-3.
               05  AUTO-SENT-FLAG      PIC X.
               05  AUTO-VIN-CHG-DATE   PIC 9(8)      COMP-3.
               05  FILLER              PIC X(30).

      *-----------------------------------------------------------------
       01  HHGS-INFORMATION-TABLE.
           03  HHGS-INFO               PIC X(120)    OCCURS HHGS-MAX.

      ******************************************************************
      *
      *    P S T - I N F O - F E E - W O R K E R S
      *
      ******************************************************************
       78  FEE-LN-MAX                                VALUE 10.
       78  FEE-MAX                                   VALUE 99.
       77  FEE-SUB                     PIC 9(02)     VALUE ZEROES.

      *-----------------------------------------------------------------
       01  FEE-CODE                    PIC 9(02)     VALUE ZEROES.
       01  FEE-CODE-NUMERIC            REDEFINES FEE-CODE.
           03  FEE-CODE-1              PIC 9(01).
           03  FEE-CODE-2              PIC 9(01).
       01  FEE-CODE-ALPHANUMERIC       REDEFINES FEE-CODE.
           03  FEE-CODE-1X             PIC X(01).
           03  FEE-CODE-2X             PIC X(01).

      *-----------------------------------------------------------------
       01  PST-INFO-FEE-AMT-WORKERS.
           03  FEE-AMT-01              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-02              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-03              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-04              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-05              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-06              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-07              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-08              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-09              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-10              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-11              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-12              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-13              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-14              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-15              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-16              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-17              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-18              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-19              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-20              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-21              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-22              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-23              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-24              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-25              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-26              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-27              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-28              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-29              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-30              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-31              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-32              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-33              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-34              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-35              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-36              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-37              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-38              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-39              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-40              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-41              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-42              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-43              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-44              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-45              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-46              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-47              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-48              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-49              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-50              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-51              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-52              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-53              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-54              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-55              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-56              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-57              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-58              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-59              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-60              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-61              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-62              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-63              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-64              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-65              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-66              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-67              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-68              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-69              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-70              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-71              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-72              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-73              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-74              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-75              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-76              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-77              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-78              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-79              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-80              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-81              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-82              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-83              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-84              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-85              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-86              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-87              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-88              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-89              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-90              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-91              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-92              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-93              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-94              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-95              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-96              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-97              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-98              PIC 9(04)V99  VALUE ZEROES.
           03  FEE-AMT-99              PIC 9(04)V99  VALUE ZEROES.

       01  PST-INFO-FEE-AMT-TABLE    REDEFINES PST-INFO-FEE-AMT-WORKERS.
           03  FEE-AMT                 PIC 9(04)V99  OCCURS FEE-MAX.

      *-----------------------------------------------------------------
       01  PST-INFO-FEE-TXT-WORKERS.
           03  FEE-TXT-01              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-02              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-03              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-04              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-05              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-06              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-07              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-08              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-09              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-10              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-11              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-12              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-13              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-14              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-15              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-16              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-17              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-18              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-19              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-20              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-21              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-22              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-23              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-24              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-25              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-26              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-27              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-28              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-29              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-30              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-31              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-32              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-33              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-34              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-35              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-36              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-37              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-38              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-39              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-40              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-41              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-42              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-43              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-44              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-45              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-46              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-47              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-48              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-49              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-50              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-51              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-52              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-53              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-54              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-55              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-56              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-57              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-58              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-59              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-60              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-61              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-62              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-63              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-64              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-65              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-66              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-67              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-68              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-69              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-70              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-71              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-72              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-73              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-74              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-75              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-76              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-77              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-78              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-79              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-80              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-81              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-82              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-83              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-84              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-85              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-86              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-87              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-88              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-89              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-90              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-91              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-92              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-93              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-94              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-95              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-96              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-97              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-98              PIC X(15)     VALUE SPACES.
           03  FEE-TXT-99              PIC X(15)     VALUE SPACES.

       01  PST-INFO-FEE-TXT-TABLE    REDEFINES PST-INFO-FEE-TXT-WORKERS.
           03  FEE-TXT                 PIC X(15)     OCCURS FEE-MAX.

      ******************************************************************
      *
      *    P S T - I N F O - I N S - W O R K E R S
      *
      ******************************************************************
       78  INS-MAX                                   VALUE 8.
       77  INS-SUB                     PIC 9(01)     VALUE ZEROES.

      *--------------------------------------------------{ INS-MAXCOVR }
       01  INS-MAXCOVR-INFORMATION.
           03  INS-MAXCOVR             PIC 9(7)V99   OCCURS INS-MAX.

       01  FILLER                     REDEFINES INS-MAXCOVR-INFORMATION.
           03  INS-MAXCOVR-CL          PIC 9(7)V99.
           03  INS-MAXCOVR-AH          PIC 9(7)V99.
           03  INS-MAXCOVR-PP          PIC 9(7)V99.
           03  INS-MAXCOVR-O1          PIC 9(7)V99.
           03  INS-MAXCOVR-O2          PIC 9(7)V99.
           03  INS-MAXCOVR-O3          PIC 9(7)V99.
           03  INS-MAXCOVR-O4          PIC 9(7)V99.
           03  INS-MAXCOVR-O5          PIC 9(7)V99.

      *-----------------------------------------------------{ INS-TERM }
       01  INS-TERM-INFORMATION.
           03  INS-TERM                PIC 9(03)     OCCURS INS-MAX.

       01  FILLER                      REDEFINES INS-TERM-INFORMATION.
           03  INS-TERM-CL             PIC 9(03).
           03  INS-TERM-AH             PIC 9(03).
           03  INS-TERM-PP             PIC 9(03).
           03  INS-TERM-O1             PIC 9(03).
           03  INS-TERM-O2             PIC 9(03).
           03  INS-TERM-O3             PIC 9(03).
           03  INS-TERM-O4             PIC 9(03).
           03  INS-TERM-O5             PIC 9(03).

      *------------------------------------------------{ MISCELLANEOUS }
       01  MONTHLY-DISABILITY-BENEFIT  PIC 9(07)V99  VALUE ZEROES.

      ******************************************************************
      *
      *    P S T - I N F O - L O A N - W O R K E R S
      *
      ******************************************************************
       01  PRIOR-ACCTNO                PIC 9(08)     VALUE ZEROES.
       01  FILLER                      REDEFINES PRIOR-ACCTNO.
           03  PRIOR-ACCTNO-NUMBER     PIC 9(08).

       01  PRIOR-ACCTNO-PAYOFF         PIC S9(7)V99  VALUE ZEROES.

      ******************************************************************
      *
      *    P S T - I N F O - P A Y M E N T - W O R K E R S
      *
      *-----------------------------------------------------------------
      *    'OT' = (VSI) VEHICLE SERVICE AGREEMENT
      *    'RA' = (AH)  ACCIDENT & HEALTH
      *    'RC' = (CL)  CREDIT LIFE
      *    'RI' = (INT) INTEREST
      *    'RM' = (MF)  MAINTENANCE FEE
      *    'RP' = (PP)  PERSONAL PROPERTY
      *    'RS' = (SV)  SERVICE CHARGE
      *    'R1' = (O1)  OTHER 1 INS.
      *    'R2' = (O2)  OTHER 2 INS.
      *    'R3' = (O3)  OTHER 3 INS.
      ******************************************************************
       01  PAYOFF-AMT-APP-LATECHG      PIC S9(7)V99  COMP-3.
       01  PAYOFF-AMT-BALANCE          PIC S9(7)V99  COMP-3.
       01  PAYOFF-AMT-PB               PIC S9(7)V99  COMP-3.
       01  PAYOFF-AMT-PO               PIC S9(7)V99  COMP-3.
       01  PAYOFF-AMT-RB               PIC S9(7)V99  COMP-3.
       01  PAYOFF-AMT-RN               PIC S9(7)V99  COMP-3.
       01  PAYOFF-AMT-RO               PIC S9(7)V99  COMP-3.
       01  PAYOFF-AMT-SC               PIC S9(7)V99  COMP-3.

      *------------------------------------------------------{ REFUNDS }
       01  REFUND-AMT-BALANCE          PIC S9(7)V99  COMP-3.

      *------------------------------------------------{ REFUND OT-VSI }
       01  REFUND-AMT-OT-VSI           PIC S9(7)V99  COMP-3.
       01  REFUND-MANUAL-OT-VSI        PIC X(01).

      *-------------------------------------------------{ REFUND RA/AH }
       01  REFUND-AMT-RA               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-AH               REDEFINES REFUND-AMT-RA
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-RA            PIC X(01).
       01  REFUND-MANUAL-AH            REDEFINES REFUND-MANUAL-RA
                                       PIC X(01).

      *-------------------------------------------------{ REFUND RC/CL }
       01  REFUND-AMT-RC               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-CL               REDEFINES REFUND-AMT-RC
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-RC            PIC X(01).
       01  REFUND-MANUAL-CL            REDEFINES REFUND-MANUAL-RC
                                       PIC X(01).

      *------------------------------------------------{ REFUND RI/INT }
       01  REFUND-AMT-RI               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-INT              REDEFINES REFUND-AMT-RI
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-RI            PIC X(01).
       01  REFUND-MANUAL-INT           REDEFINES REFUND-MANUAL-RI
                                       PIC X(01).

      *-------------------------------------------------{ REFUND RM/MF }
       01  REFUND-AMT-RM               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-MF               REDEFINES REFUND-AMT-RM
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-RM            PIC X(01).
       01  REFUND-MANUAL-MF            REDEFINES REFUND-MANUAL-RM
                                       PIC X(01).

      *-------------------------------------------------{ REFUND RP/PP }
       01  REFUND-AMT-RP               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-PP               REDEFINES REFUND-AMT-RP
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-RP            PIC X(01).
       01  REFUND-MANUAL-PP            REDEFINES REFUND-MANUAL-RP
                                       PIC X(01).

      *-------------------------------------------------{ REFUND RS/SV }
       01  REFUND-AMT-RS               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-SV               REDEFINES REFUND-AMT-RS
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-RS            PIC X(01).
       01  REFUND-MANUAL-SV            REDEFINES REFUND-MANUAL-RS
                                       PIC X(01).

      *-------------------------------------------------{ REFUND R1/O1 }
       01  REFUND-AMT-R1               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-O1               REDEFINES REFUND-AMT-R1
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-R1            PIC X(01).
       01  REFUND-MANUAL-O1            REDEFINES REFUND-MANUAL-R1
                                       PIC X(01).

      *-------------------------------------------------{ REFUND R2/O2 }
       01  REFUND-AMT-R2               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-O2               REDEFINES REFUND-AMT-R2
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-R2            PIC X(01).
       01  REFUND-MANUAL-O2            REDEFINES REFUND-MANUAL-R2
                                       PIC X(01).

      *-------------------------------------------------{ REFUND R3/O3 }
       01  REFUND-AMT-R3               PIC S9(7)V99  COMP-3.
       01  REFUND-AMT-O3               REDEFINES REFUND-AMT-R3
                                       PIC S9(7)V99  COMP-3.

       01  REFUND-MANUAL-R3            PIC X(01).
       01  REFUND-MANUAL-O3            REDEFINES REFUND-MANUAL-R3
                                       PIC X(01).

      ******************************************************************
      *
      *    P S T - I N F O - M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  PST-INFO-FIRST-TIME           PIC X(01)     VALUE "Y".

       01  PST-INFO-FORCE-JOINT-BORROWER PIC 9(01)     VALUE ZEROES.
           88  PST-INFO-FORCE-JOINT-BW-NONE            VALUE 0.
           88  PST-INFO-FORCE-JOINT-BW-2ND             VALUE 2.
           88  PST-INFO-FORCE-JOINT-BW-3RD             VALUE 3.
           88  PST-INFO-FORCE-JOINT-BW-4TH             VALUE 4.

       01  PST-INFO-ORIGINAL-RECORD-LN   PIC X(LN-SIZE).
       01  PST-INFO-ORIGINAL-RECORD-SP   PIC X(SP-SIZE).
       01  PST-INFO-PST-PAGE-MAX         PIC 9(02)     VALUE 01.
       01  PST-INFO-PST-PAGE-NUMBER      PIC 9(02)     VALUE 01.
       01  PST-INFO-PST-PAGE-STAT        PIC X(02)     VALUE "00".
