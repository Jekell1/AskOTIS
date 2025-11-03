      * COPYMEMBER: LIBLP/LPPDCL
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPPDCL
      *
      * DESCRIPTION   :  SCRATCH PAD FILE (PD) CLEAR ROUTINE
      *
      *-----------------------------------------------------------------
      * REV:
      * JTG 033195 REMOVED REF: NOODDPY
      * JTG 061995 ADDED PD-O3INS-DLPROC
      * JTG 070995 ADDED PD-INSPREM-DLADJ, ADJUSTMENT WHEN DEALER
      *                  WROTE OUR INS. PREMIUMS WRONG
      * JTG 071995 ADDED PD-PRLNAMT & PD-PRPDUE-DAYS
      * JTG 072095 ADDED PD-PLAMT = CURBAL AT TIME OF P&L
      * JTG 082495 ADDED PD-MTD-PRIN, PD-MTD-LC, PD-MTD-INT
      *                  PD-MTD-OT, PD-MTD-EX
      * KEC 083095 ADDED PD-BNKRPT-CHAPTER
      * JTG 092295 ADDED PD-BMO-CON-STATUS PD-BMO-REC-STATUS
      * KEC 030696 ADDED PD-APPROVAL-LIMIT PD-APPROVAL-USERID.
      * JTG 111296 CHANGED PD-INSTBL (LEVEL'S 9 & 10) TO FILLER
      * BLV 022597 INITIALIZE FIELDS FOR CREDIT SCORE APPROVAL
      * KEC 051997 ADDED PD-APPNO.
      * MJD 071597 CHANGED PD-CAN-LOAN-NO TO PIC XX.
      * JTG 082097 ADDED PD-GIFTNET-OUR-PO PD-GIFTNET-SSNO
      *                  PD-SHIP-PHONE     PD-GIFTNET-DATE(1) THRU (4)
      *                  PD SIXTY MONTH PROFILE TABLES
      *            ADDED LOGIC TO CLEAR 60 MONTH PROFILE TABLES
      *            ADDED PD-PY-YYYYMM-RECDEL & PD-MTD-PAYS-RECDEL
      *            ADDED PD-AD-D-INS-COVR   & PD-APR-FINAMT
      * BAH 091297 CHANGED PD-SPREFUNDATE TO PD-TRW-1ST-30
      *  CS 031998 ADDED CLEAR OF PD-FOLLOWUP-DATE
      * JTG 050498 ADDED PD-TOT-PARTIAL-CHGOFF, MERCURY TRCD = 'PX'
      * BAH 062598 CHANGED PD-TILLNO TO ALPHA, REMOVED THE MOVE 0
      * JTG 071598 ADDED PD-PRDLDISC-UNEARN TO HOLD CARRY OVER DISCOUNT
      *             ON RENEWALS
      * BAH 081098 ADDED PD-YTDNOALDEL FOR MERCURY'S NEW EDIT ON
      *             ALLOWABLE DELINQUENCY, #193
      * JTG 082698 ADDED CLEAR OF LN-PREV-STMT-DATE,
      *             REGENCY FINANCE REVOLVING
      *                            LN-LAST-STMT-DATE
      *                            LN-RPRINBAL
      *                            LN-AGEING-AMT    OCCURS 8
      * JTG 101598 CHANGED PD-CYCLE-DAY TO PD-CYCLE-DATE REGENCY, REVOL
      * KEC 102898 ADDED PD-INDEX-ADJUST-FACTOR(1 & 2) REGENCY REVOLVIN
      * JTG 111998 ADDED PD-OTHBAL-TABLE REVOLVING, REGENCY
      * JTG 112398 ADDED PD-RCURPRIN, PD-RCURINT, PD-RCURLC, PD-RCUROTH
      *             AND REMOVED PD-RPRINBAL
      * JTG 120198 ADDED PD-RPDUEPRIN, PD-RPDUEINT, PD-RPDUELC,
      *             PD-RPDUEOTH
      * JTG 990121 ADDED SET OF PD-CHARGE-INTEREST-FG = "Y"
      *             ADDED SET OF PD-CHARGE-LATECHG-FG = "Y"
      *             ADDED SET OF PD-CHARGE-MEMBERSHIP-FG = "Y"
      * JTG 990301 ADDED LN-ZERO-BAL-CNT, REVOLVING
      * JTG 990326 ADDED LN-LAST-ACCRUAL-DATE, SP-ERNDELQFRMLA = 'C'
      *             TFC #005
      * MJD 991005 ADDED PD-PRIOR-RN-FOR10 AND PD-LAST-RN-FOR10.
      *             (REGMGM PR#942)
      * KEC 991018 ADDED PD-CREATION-STATUS FOR CMSI/CSI PROJECT (LI)
      *              88  PD-CREATION-EXTERNAL            VALUE "L".
      *              88  PD-CREATION-INTERNAL            VALUE "I" " ".
      *              88  PD-CREATION-LI-VENDOR           VALUE "L".
      * MJD 991230 ADDED PD-TOTNODEFACT AND PD-YTDNODEFACT(WORLD PR#216)
      * MJD 000119 ADDED PD-TOTNOLCHG (WORLD PR#218)
      * BLV 000313 ADDED PD-WCBC-CREDITLIM (WORLD PR#225)
      * MJD 000329 ADDED PD-PRIOR-STMT-COUNTER
      *
      * KEC 2000.0509 {PR#00000} PARADATA FINANCIAL [JOHN]
      *                JOHN WANTED ALL 'PD' FIELDS TO BE ZEROED OUT.
      *                REVISED SO TO GET ALL FIELDS.
      * MJD 000620 ADDED PD-CONRATE (REGACC PR#1220)
      * JTG 010227 ADDED LN-EXTRA-PRIN-LFP LN-REMAIN-INT-SINCE-LFP
      *            LN-REMAIN-PRIN-SINCE-LFP  (LFP=LAST-FULL-PAYMENT)
      *            LENDMARK, PR#1045 & PR#1380
      *  BAH 020218 ADDED PD-LAST-PYPA-DATE FOR REGACC PR# 1721
      *  BAH 020502 ADDED PD-PROMDAYS-PASTDUE, REGACC PR# 1783
      *  JTG 020530 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED PD-MIP-MTD-CLAHPAID, PD-MIP-TOT-CLPAID,
      *                   PD-MIP-TOT-AHPAID,
      *                   PD-MIP-CLPDTH,   PD-MIP-AHPDTH
      *  KEC 020806 {DR#00009} PARADATA <A15> [KEN]
      *             ADDED PD-RESCISSION AND PD-RESCISSION-DATE.
      * KEC 2016.0510 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED PD-APPNO TO USE PD-APP-NUMBER FOR MA1-KEY.
      *  BLM 160623 FIX FOR NEW FILE LAYOUT, PD0003
      *   CS 190507 CLEAR FUTURE FILES
      * BAH 20190627 REMOVED PD-TAX-AMT, PD-ZERO-BAL-CNT,
      *              PD-PREV-STMT-DATE PD-LAST-STMT-DATE PT0153
      * BAH 2024.0404 ADDED PD-LOS-APP-ID
      ******************************************************************
       CLEAR-PD-FILE SECTION.
           MOVE SPACES TO PD-RECORD.
           MOVE 0 TO PD-BRANCH PD-SOC-SEC.

           MOVE 0 TO
                PD-OWNBR PD-ACCTNO PD-CLASS PD-SUBCLASS
                PD-LAWCODE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                   MOVE 0 TO PD-SSNO(SUB)
           END-PERFORM.

           MOVE 0 TO
                PD-ORGBR                    PD-SPRCLASS
                PD-LC-CODE                  PD-ENTDATE
                PD-LOANDATE                 PD-PURDATE
                PD-INTDATE                  PD-CASHDATE
                PD-FILEDATE                 PD-RESCISSION-DATE
                PD-PART-RECOURSE-DATE       PD-ORIG-1STPYDATE 
                PD-1STPYDATE                PD-JDDATE
                PD-PLDATE                   PD-PL-TRDATE
                PD-ACCELDATE                PD-REDUDATE
                PD-POFFDATE                 PD-PO-TRDATE
                PD-LEGALDATE                PD-BNKRPTDATE
                PD-BNKRPT-DISMISS-DATE      PD-BNKRPT-DISCHARGE-DATE
                PD-BNKRPT-REAFFIRM-DATE     PD-DEBTAIDATE
                PD-REPODATE                 PD-REDEEMDATE
                PD-SOLDDATE                 PD-INS-CONFIRM-DATE
                PD-PRIOR-RN-FOR10-DATE      PD-LAST-RN-FOR10-DATE
                PD-TRW-DOFD-DATE            PD-LAST-PYPA-DATE
                PD-BT-TRDATE                PD-METRO2-LAST-PMT-DATE
                PD-FOLLOWUP-DATE            PD-LAST-ACCRUAL-DATE
                PD-APRATE                   PD-EFFRATE 
                PD-SMPRATE                  PD-YLDRATE 
                PD-OVRDRATE                 PD-ENDORATE 
                PD-JDRATE                   PD-ACCRATE
                PD-REDURATE                 PD-CONRATE
                PD-SERVPER                  PD-SELLPR
                PD-DOWNPY                   PD-TRADEIN
                PD-LNAMT                    PD-FINCHG
                PD-INTCHG                   PD-SERCHG
                PD-EXTCHG                   PD-MAINTFEE
                PD-POINTS                   PD-PROCEEDS
                PD-ORGTERM                  PD-1STPYAMT
                PD-NOREGPY                  PD-REGPYAMT
                PD-LASTPYAMT                PD-PROCASH
                PD-TILLNO
                PD-STATE-WORKER             PD-STATE-WORKER2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
                   MOVE 0  TO PD-FEEAMT(SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                   MOVE 0                TO PD-INSCOMP      (SUB)
                                            PD-INSEFF-DATE  (SUB)
                                            PD-INSEXP-DATE  (SUB)
                                            PD-INSPREM      (SUB)
                                            PD-INSCOVR      (SUB)
                                            PD-INSCOMM      (SUB)
                                            PD-INSANTICERN  (SUB)
                                            PD-INSANTICADJ  (SUB)
                                            PD-INSUFERN     (SUB)
                                            PD-INSACCERN    (SUB)
                                            PD-INSEARN-ORDER(SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO
                PD-PY-YYYYMM-RECDEL         PD-MTD-PAYS-RECDEL
                PD-AD-D-INS-COVR            PD-TOT-PARTIAL-CHGOFF
                PD-EMP-ID
                PD-WCBC-CREDITLIM           
                PD-MIP-MTD-CLAHPAID         PD-MIP-CLPDTH-DATE
                PD-MIP-TOT-CLPAID           PD-MIP-AHPDTH-DATE
                PD-MIP-TOT-AHPAID
                PD-IL-GROSS-INCOME          PD-IL-GROSS-INCOME2
                PD-DLNO                     PD-DLPOOL
                PD-DLPARTRCAMT              PD-ORIGPURAMT
                PD-PURBAL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
                   MOVE 0                TO PD-DLRVPRIN(SUB)
                                            PD-DLRVCHGS(SUB)
           END-PERFORM.

           MOVE 0 TO
                PD-DLPARVHELD               PD-DLPARVPAID
                PD-DLDISC                   PD-LCPDTH-DATE
                PD-INTPDTH-DATE             PD-CURBAL
                PD-INTBAL                   PD-LCBAL
                PD-OTHBAL                   PD-OT2BAL
                PD-LCPAID                   PD-LCPARTIALS
                PD-STMT-COUNTER             PD-PRIOR-STMT-COUNTER
                PD-MONTHEND-BAL             PD-ANTICTERM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                   MOVE 0                TO PD-ANTICERN  (SUB)
                                            PD-ANTICADJ  (SUB)
                                            PD-UFERN     (SUB)
                                            PD-ACCERN    (SUB)
                                            PD-EARN-ORDER(SUB)
           END-PERFORM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 0 TO
                PD-LMO-PRIOR-INTEREST       PD-PRIOR-UNEARNED
                PD-NONACRERN                PD-PRIOR-NONACRERN
                PD-ERN-IB-PDTH-DATE         PD-ERN-IB-PRINBAL
                PD-ERN-IB-INTOWE            PD-TOTERN
                PD-TOTOTH                   PD-TOTINT
                PD-TOTLCHG                  PD-TOTDEF
                PD-TOTSERACCT               PD-LCWAIVERS
                PD-INTWAIVERS               PD-TOTPAYMNTD
                PD-TOTEXCPAYMNTD            PD-TOTNODEF
                PD-YTDNODEF                 PD-TOTNODEFACT
                PD-YTDNODEFACT              PD-TOTNOALDEL
                PD-YTDNOALDEL               PD-YTDINTPD 
                PD-LYRINTPD                 PD-TOTINTPD
                PD-RECASTINT                PD-RECASTPRIN               
                PD-PROMDAYS-PASTDUE         PD-PROMDATE  
                PD-PROMAMT                  PD-PROMDATE2
                PD-DATE-PAID-LAST           PD-AMT-PAID-LAST
                PD-LASTCFORMNO              PD-LASTCFORMDATE
                PD-DELCNT                   PD-DELCNT2
                PD-CREDITLIM                PD-CRSCORE
                PD-LASTSFORMNO              PD-LASTSFORMDATE
                PD-SOLCNT                   
                PD-OLDACCTNO                PD-PRLNNO
                PD-PRBAL                    PD-PRINTDUE
                PD-PRFINCHGRB               PD-PRINSRB
                PD-PRPODATE                 PD-PRDELINQAMT 
                PD-PRLNAMT                  PD-PRPDUE-DAYS
                PD-PRDLDISC-UNEARN          PD-SUM-ORIGPURAMT
                PD-SUM-LNAMT                PD-SUM-FINCHG
                PD-SUM-INTCHG               PD-SUM-ORGTERM
                PD-SUM-REGPYAMT             PD-SUM-EXTCHG
                PD-SUM-SERCHG               PD-SUM-MAINTFEE
                PD-UNITPER-FREQ             PD-DAYS-IN-UNITPER
                PD-UNITPER-PER-YEAR         PD-O3INS-DLPROC
                PD-INSPREM-DLADJ            PD-PLAMT
                PD-MTD-PRIN                 PD-MTD-LC
                PD-MTD-INT                  PD-MTD-OT
                PD-MTD-EX                   PD-BNKRPT-CHAPTER
                PD-APP-NUMBER               PD-GIFTNET-OUR-PO
                PD-NOODDPY                  
                PD-NEXIS-CRSCORE            PD-TOTNOLCHG
                PD-EXTRA-PRIN-LFP           PD-REMAIN-INT-SINCE-LFP
                PD-REMAIN-PRIN-SINCE-LFP
                PD-DLCOMM-O3-CMSI           PD-DLCOMM-PP-CMSI
                PD-APPROVED-BY-DATE         PD-APPROVED-BY-TIME
                PD-LTOUCH-DATE              PD-LTOUCH-TIME
                PD-LOS-APP-ID.

 
           MOVE 0 TO
                PD-CKNO                     PD-POSTING-BR
                PD-APPROVAL-LIMIT           PD-MIN-CRED-SCORE
                PD-FINAMT
                PD-PRPAID-AHEAD             PD-RESTRICT-ADJ
                PD-DUAL-LOAN-ADJ            PD-CAN-LOAN-MAX
                PD-PERDOWN                  PD-APR-FINAMT.

           MOVE 0 TO PD2-BRANCH.

      *---------------------------------------------{ DEFAULT SETTINGS }

           MOVE "9"                      TO PD-BMO-CON-STATUS.
           MOVE "9"                      TO PD-BMO-REC-STATUS.
           MOVE "I"                      TO PD-CREATION-STATUS.
           MOVE 30                       TO PD-DAYS-IN-UNITPER.
           MOVE 9999                     TO PD-MIN-CRED-SCORE.
           MOVE 999999999999             TO PD-PRCONPROFILE.
           MOVE "N"                      TO PD-RESCISSION.
           MOVE "M"                      TO PD-UNITPER-CD.
           MOVE 1                        TO PD-UNITPER-FREQ.
           MOVE 12                       TO PD-UNITPER-PER-YEAR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
                   MOVE 999999999999     TO PD-CONPROFILE(SUB)
                                            PD-RECPROFILE(SUB)
           END-PERFORM.

       CLEAR-PD-FILE-EXIT.
           EXIT.
