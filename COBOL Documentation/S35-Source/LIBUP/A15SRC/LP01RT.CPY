      * COPYMEMBER: LIBLP/LP01RT
      **************************************************************************
      *
      *   IDENTIFICATION: LIBLP/LP01RT                      BYTES = 200
      *
      *   DESCRIPTION   : REVOLVING TRANSACTION FILE RECORD
      *
      *-----------------------------------------------------------------
      *   KEY 1 -  ACCTNO, SEQNO
      *   KEY 2 -  ACCTNO, TYPE, STATUS, TRDATE
      *   KEY 3 -  ACCTNO, TRDATE, SEQNO
      *
      *-----------------------------------------------------------------
      *   RT-TRDATE [ 'YYYYMMDD' FORMAT ]
      *-----------------------------------------------------------------
      * REV:
      * KEC 100898 ***NEW***                               BYTES = 200
      * MJD 990122 FILE SIZE IS 214                        BYTES = 214
      * KEC 020199 CHANGE THE FOLLOWING FROM '9(8)' TO '9(6)':
      *              RT-ST-PRIOR-STMTDATE   RT-ST-STMTDATE
      * JTG 990201 ADDED RT-PY-NOT-APPLIED
      * JTG 990202 REARRANGED KEY 2  A, T, T, S   TO   A, T, S, T
      * JTG 990212 ADDED RT-PY-REV-FG
      * KEC 990223 ADDED RT-ST-AGEING-BUCKETS.
      * JTG 990701 REWORKER 'PU' RECORD FOR DIRECT LOAN ADVANCES 
      * JTG 990728 REMOVED RT-AD-ADJ-LCBAL & RT-AD-ADJ-INTBAL
      * JTG 990927 ADDED FOR 'AP' APPLY TO PRINCIPAL TRANSACTION
      *              RT-AD-ADJ-RT-PU-SEQNO RT LINK 'AP' TO PU RT-REC
      *              RT-PU-ADJ-RT-AD-SEQNO RT LINK 'AP' TO AD RT-REC
      * GLB 000209 ADDED RT-PU-MIN-PMT; RT-PU-PMT-REQUIRED-FG
      * GLB 000216 EXPANDED RT-PU-PMT-REQUIRED-FG FROM 1 TO 2 CHAR
      *  CS 090330 ADDED RT-PU-CAPTIZ-THRUPROD(NEEDED TO CALCULATE AN ADB ON
      *            THE NEXT STATEMENT RUN AFTER CONTGNT INTERST CAPITALIZED
      *                                               REGENCY PR#3215
      ******************************************************************
       78  RT-SIZE                                            VALUE 214.
      *-----------------------------------------------------------------
       01  RT-REC.
           03  RT1-KEY.
               05  RT-ACCTNO               PIC 9(8).
               05  RT-SEQNO                PIC 9(5)  COMP-3.

      *-----------------------------------------------------------------
      *    NON-CONTIGUOUS KEY(S):
      *     RT2-KEY = RT-ACCTNO RT-TYPE RT-STATUS RT-TRDATE   WITH DUP.
      *     RT3-KEY = RT-ACCTNO RT-TRDATE RT-SEQNO            WITH DUP.
      *-----------------------------------------------------------------
           03  RT-COMMON-INFORMATION.
               05  RT-BRNO                        PIC 9(4)       COMP-3.
               05  RT-CLASS                       PIC 9(2).

               05  RT-NUM                         PIC 9(6).
               05  FILLER                         REDEFINES RT-NUM.
                   07  RT-DL                      PIC 9(4).
                   07  RT-DLBR                    PIC 9(2).
               05  RT-DLNO                        REDEFINES RT-NUM
                                                  PIC 9(4)V99.

               05  RT-POSTDATE                    PIC 9(6)       COMP-3.
               05  RT-POSTTIME                    PIC 9(6)       COMP-3.
      *-------------------------------------------------------------------------
      *  ADVANCES :
      *  PURCHASES:
      *            ' '  OPEN   -  SET WHEN ADVANCE/PURCHASE IS PUT ON THE SYSTEM
      *            'CL' CLOSED -  SET DURING STATEMENT RUN WHEN ADVANCE/PURCHASE
      *                           HAS A ZERO BALANCE
      *  PAYMENTS :
      *            ' '  OPEN   -  SET WHEN PAYMENT HAS NOT BEEN FULLY
      *                           APPLIED TO ADVANCE/PURCHASE
      *            'CL' CLOSED -  SET WHEN PAYMENT HAS BEEN FULLY APPLIED,
      *                           EITHER:
      *                            1. AT THE TIME OF BOOKING THE PAYMENT
      *                            2. WHEN AN ADVANCE/PURCHASE WAS BOOKED 
      *                               AFTER THE REMAINING CREDIT WAS APPLIED
      *-------------------------------------------------------------------------
               05  RT-STATUS                      PIC X(2).
                   88  RT-STATUS-CLOSE                       VALUE "CL".
                   88  RT-STATUS-OPEN                        VALUE "  ".
                   88  RT-STATUS-VALID                       VALUE
                                                              "CL" "  ".

               05  RT-TRDATE                      PIC 9(8)       COMP-3.

               05  RT-TYPE                        PIC X(2).
                   88  RT-TYPE-ADJUSTMENT                    VALUE "AD".
                   88  RT-TYPE-PAYMENT                       VALUE "PY".
                   88  RT-TYPE-PURCHASE                      VALUE "PU".
                   88  RT-TYPE-STATEMENT                     VALUE "ST".
                   88  RT-TYPE-VALID                         VALUE
                                                  "AD" "PY" "PU" "ST".

               05  RT-USERID                      PIC X(10).

      *=================================================================
           03  RT-RECORD                          PIC X(165).

      ******************************************************************
      *
      *    A D J U S T M E N T  -  R E C O R D
      *
      ******************************************************************
           03  RT-ADJUSTMENT-RECORD             REDEFINES RT-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *       LINK TO 'LP-REC' RECORD
               05  RT-AD-LP-SEQNO              PIC 9(4)       COMP-3.

      *-------------------------------------------------------------------------
      * THIS TABLE CORRESPONDS TO LN-OTHBAL-TABLE:
               05  RT-AD-ADJ-TABLE.
                   07  RT-AD-ADJ-AMT          OCCURS 12
                                               PIC S9(7)V99   COMP-3.
               05  FILLER                     REDEFINES RT-AD-ADJ-TABLE.
                   07  RT-AD-ADJ-CL            PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-AH            PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-PP            PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-O1            PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-MEMBERSHIP    PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-NSF           PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-MISC1         PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-MISC2         PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-MISC3         PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-MISC4         PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-MISC5         PIC S9(7)V99   COMP-3.
                   07  RT-AD-ADJ-MISC6         PIC S9(7)V99   COMP-3.

               05  RT-AD-ADJ-RCURPRIN          PIC S9(7)V99   COMP-3.
               05  RT-AD-ADJ-RCURINT           PIC S9(7)V99   COMP-3.
               05  RT-AD-ADJ-RCURLC            PIC S9(7)V99   COMP-3.
               05  RT-AD-ADJ-RCUROTH           PIC S9(7)V99   COMP-3.

               05  RT-AD-ADJ-RPDUEPRIN         PIC S9(7)V99   COMP-3.
               05  RT-AD-ADJ-RPDUEINT          PIC S9(7)V99   COMP-3.
               05  RT-AD-ADJ-RPDUELC           PIC S9(7)V99   COMP-3.
               05  RT-AD-ADJ-RPDUEOTH          PIC S9(7)V99   COMP-3.

               05  RT-AD-ADJ-AGEING-BUCKETS.
                   07  RT-AD-ADJ-AGEING-AMT   OCCURS 8
                                               PIC S9(7)V99   COMP-3.

      *-----------------------------------------------------------------
               05  RT-AD-ADJ-REV-FG            PIC X.
               05  RT-AD-ADJ-RT-PU-SEQNO       PIC 9(5)       COMP-3.
               05  FILLER                      PIC X(18).

      ******************************************************************
      *                 P U R C H A S E   R E C O R D
      *    RT-PU-TRCD : 
      *          PU -  PURCHASE               SALES FINANCE REVOLVING
      *          AV -  ADVANCE                DIRECT LOAN REVOLVING
      ******************************************************************
           03  RT-PURCHASE-RECORD                 REDEFINES RT-RECORD.
               05  RT-PU-TRCD                     PIC X(2).
      *-----------------------------------------------------------------
      *       LINK TO 'LP-REC' RECORD
               05  RT-PU-LP-SEQNO                 PIC 9(4)       COMP-3.

      *-----------------------------------------------------------------
               05  RT-PU-DLDISC                   PIC S9(7)V99   COMP-3.
               05  RT-PU-SERCHG                  REDEFINES RT-PU-DLDISC
                                                  PIC S9(7)V99   COMP-3.

               05  RT-PU-DLRVPRIN1                PIC S9(7)V99   COMP-3.
               05  RT-PU-DLPARVPAID               PIC S9(7)V99   COMP-3.
               05  RT-PU-DLPARVHELD               PIC S9(7)V99   COMP-3.

               05  RT-PU-DLPROC                   PIC S9(7)V99   COMP-3.
               05  RT-PU-PROCEEDS                REDEFINES RT-PU-DLPROC
                                                  PIC S9(7)V99   COMP-3.

               05  RT-PU-TOTINT                   PIC S9(7)V99   COMP-3.
      *-----------------------------------------------------------------
               05  RT-PU-PURAMT                   PIC S9(7)V99   COMP-3.
               05  RT-PU-PURBAL                   PIC S9(7)V99   COMP-3.

      *-----------------------------------------------------------------
      *    SALES FINANCE REVOLVING:  ANTICIPATED DISCOUNT EARNINGS
      *    DIRECT LOAN REVOLVING  :  ANTICIPATED SERVICE CHARGE EARNINGS
      *-----------------------------------------------------------------
               05  RT-PU-ANTICTERM                PIC 9(3)       COMP-3.
               05  RT-PU-ANTICERN                 PIC S9(6)V99   COMP-3.
               05  RT-PU-UFERN                    PIC S9(6)V99   COMP-3.
               05  RT-PU-ACCERN                   PIC S9(6)V99   COMP-3.

      *-----------------------------------------------------------------
      *    ALL 9'S INDICATES STANDARD RATES, ELSE OVERRIDE
      *-----------------------------------------------------------------
               05  RT-PU-RATE                     PIC 999V999    COMP-3.

      *-----------------------------------------------------------------
               05  RT-PU-PURDATE                  PIC 9(6)       COMP-3.
               05  RT-PU-INTDATE                  PIC 9(6)       COMP-3.
               05  RT-PU-CASHDATE                 PIC 9(6)       COMP-3.

      *-----------------------------------------------------------------
               05  RT-PU-PRODUCT-CODE             PIC X(5).
               05  RT-PU-REFNO                    PIC X(15).
               05  RT-PU-RVPROG                   PIC X(2).
               05  RT-PU-RVPROG-LAWCODE           PIC 9(2).

      *-----------------------------------------------------------------
               05  RT-PU-ADB-THRUPROD             PIC S9(9)V99   COMP-3.
               05  RT-PU-ADB-THRUDATE             PIC 9(6)       COMP-3.

      *-----------------------------------------------------------------
      * CONTINGENT FIELDS:
               05  RT-PU-CONTGNT-INT              PIC S9(6)V99   COMP-3.
               05  RT-PU-CONTGNT-CL               PIC S9(5)V99   COMP-3.
               05  RT-PU-CONTGNT-AH               PIC S9(5)V99   COMP-3.
               05  RT-PU-CONTGNT-PP               PIC S9(5)V99   COMP-3.
               05  RT-PU-CONTGNT-O1               PIC S9(5)V99   COMP-3.
      *-----------------------------------------------------------------
               05  RT-PU-PRIORBAL                 PIC S9(7)V99   COMP-3.
               05  RT-PU-REV-FG                   PIC X.
               05  RT-PU-ADJ-RT-AD-SEQNO          PIC 9(5)       COMP-3.
               05  RT-PU-MIN-PMT-DUE              PIC S9(7)V99   COMP-3.
               05  RT-PU-PMT-REQUIRED-FG          PIC XX.
      * 03/02/00 GLB
               05  RT-PU-ROLL-INT                 PIC S9(7)V99   COMP-3.
               05  RT-PU-CAPTIZ-THRUPROD          PIC S9(9)V99   COMP-3.
                
               05  FILLER                         PIC X(5).
      *CS      05  FILLER                         PIC X(11).
      * 03/00  05  FILLER                         PIC X(16).
      * 02/09  05  FILLER                         PIC X(18).
      * #GB    05  FILLER                         PIC X(23).

      ******************************************************************
      *
      *    P A Y M E N T - R E C O R D
      *
      ******************************************************************
           03  RT-PAYMENT-RECORD                 REDEFINES RT-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *       LINK TO 'LP-REC' RECORD
               05  RT-PY-LP-SEQNO                PIC 9(4)       COMP-3.

      *-------------------------------------------------------------------------
      * THIS TABLE CORRESPONDS TO LN-OTHBAL-TABLE:
               05  RT-PY-APPLY-TABLE.
                   07  RT-PY-APPLY-AMT           OCCURS 12
                                                 PIC S9(7)V99   COMP-3.
               05  FILLER                  REDEFINES RT-PY-APPLY-TABLE.
                   07  RT-PY-APPLY-CL            PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-AH            PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-PP            PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-O1            PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-MEMBERSHIP    PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-NSF           PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-MISC1         PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-MISC2         PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-MISC3         PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-MISC4         PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-MISC5         PIC S9(7)V99   COMP-3.
                   07  RT-PY-APPLY-MISC6         PIC S9(7)V99   COMP-3.

               05  RT-PY-APPLY-RCURPRIN          PIC S9(7)V99   COMP-3.
               05  RT-PY-APPLY-RCURINT           PIC S9(7)V99   COMP-3.
               05  RT-PY-APPLY-RCURLC            PIC S9(7)V99   COMP-3.
               05  RT-PY-APPLY-RCUROTH           PIC S9(7)V99   COMP-3.

               05  RT-PY-APPLY-RPDUEPRIN         PIC S9(7)V99   COMP-3.
               05  RT-PY-APPLY-RPDUEINT          PIC S9(7)V99   COMP-3.
               05  RT-PY-APPLY-RPDUELC           PIC S9(7)V99   COMP-3.
               05  RT-PY-APPLY-RPDUEOTH          PIC S9(7)V99   COMP-3.

               05  RT-PY-APAGEING-BUCKETS.
                   07  RT-PY-APAGEING-AMT    OCCURS 8
                                                 PIC S9(7)V99   COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *       THE AMOUNT NOT APPLIED TO PURCHASE/ADVANCE RECORDS
               05  RT-PY-NOT-APPLIED             PIC S9(7)V99   COMP-3.

               05  RT-PY-REV-FG                  PIC X.
      *-----------------------------------------------------------------
               05  FILLER                        PIC X(16).


      ******************************************************************
      *
      *    S T A T E M E N T - R E C O R D
      *
      ******************************************************************
           03  RT-STATEMENT-RECORD      REDEFINES RT-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *       LINK TO 'LP-REC' RECORD
               05  RT-ST-LP-SEQNO                PIC 9(4)       COMP-3.

      *-------------------------------------------------------------------------
               05  RT-ST-CUR-DUE-PRIN             PIC S9(7)V99  COMP-3. 
               05  RT-ST-CUR-DUE-INT              PIC S9(7)V99  COMP-3. 
               05  RT-ST-CUR-DUE-LC               PIC S9(7)V99  COMP-3. 
               05  RT-ST-CUR-DUE-OT               PIC S9(7)V99  COMP-3. 

               05  RT-ST-PASTDUE-PRIN             PIC S9(7)V99  COMP-3. 
               05  RT-ST-PASTDUE-INT              PIC S9(7)V99  COMP-3. 
               05  RT-ST-PASTDUE-LC               PIC S9(7)V99  COMP-3. 
               05  RT-ST-PASTDUE-OT               PIC S9(7)V99  COMP-3. 

               05  RT-ST-CONTGNT-DUE-INT          PIC S9(7)V99  COMP-3. 
               05  RT-ST-CONTGNT-DUE-CL           PIC S9(7)V99  COMP-3. 
               05  RT-ST-CONTGNT-DUE-AH           PIC S9(7)V99  COMP-3. 
               05  RT-ST-CONTGNT-DUE-PP           PIC S9(7)V99  COMP-3. 
      *SEEBELOW05  RT-ST-CONTGNT-DUE-O1           PIC S9(7)V99  COMP-3. 

               05  RT-ST-PRIORBAL                 PIC S9(7)V99  COMP-3.
               05  RT-ST-DEBITS                   PIC S9(7)V99  COMP-3.
               05  RT-ST-CREDITS                  PIC S9(7)V99  COMP-3.
               05  RT-ST-CURBAL                   PIC S9(7)V99  COMP-3.

               05  RT-ST-AVAIL-CREDIT             PIC S9(7)V99  COMP-3.
               05  RT-ST-PRIOR-STMTDATE           PIC 9(6)      COMP-3.
               05  RT-ST-STMTDATE                 PIC 9(6)      COMP-3.
               05  RT-ST-DAYS-IN-CYCLE            PIC 9(2).
               05  RT-ST-AVG-DAILY-BAL            PIC S9(7)V99  COMP-3.

               05  RT-ST-PMT-DUEDATE              PIC 9(6)      COMP-3.
               05  RT-ST-PMT-DUEAMT               PIC S9(6)V99  COMP-3.

               05  RT-ST-PMT-LCPDTH               PIC 9(6)      COMP-3.
               05  RT-ST-PMT-LATEDATE             PIC 9(6)      COMP-3.
               05  RT-ST-PMT-LATEAMT              PIC S9(6)V99  COMP-3.


               05  RT-ST-PRINT-FG                 PIC X(1).
               05  RT-ST-UPDATE-FG                PIC X(1).

               05  RT-ST-AGEING-BUCKETS.
                   07  RT-ST-AGEING-AMT           OCCURS 8
                                                  PIC S9(6)V99   COMP.

      *-----------------------------------------------------------------
               05  RT-ST-CONTGNT-DUE-O1           PIC S9(7)V99  COMP-3. 
      *-----------------------------------------------------------------
               05  FILLER                         PIC X.

