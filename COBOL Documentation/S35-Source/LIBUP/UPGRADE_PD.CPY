      ******************************************************************
      *
      *    U P G R A D E - F I L E - P D
      *
      *=================================================================
      *    IN  : OPD-REC
      *
      *    OUT : PD-REC
      *
      *    DESC: UPGRADE SOURCE RECORD TO TARGET RECORD
      *          A15 TO A30
      *
      *=================================================================
      * REV:
      * BLM 170309 CHANGED PERFORMS OF CONVERT-MMDDYY-TO-YYYYMMDD TO
      *            CONVERT-MMDDYY-TO-YYYYMMDD, PD0006
      *
      * KEC 2017.0616 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED LOGIC FOR PD-BNKRPT-DISMISS-DATE,
      *          PD-PY-YYYYMM-RECDEL, PD-GARNISH-EXTR,
      *          PD-EMP-ID & PD-REP-CD, PD-APP-NUMBER,
      *          PD-ZERO-BAL-CNT & PD-NEXIS-CRSCORE.
      * BAH 2018.0430 PD-MIP-TOT-CLPAID AND AHPAID WERE 202020 BECAUSE
      *               WE NEVER CLEARED THEM IN A15...
      *
      ******************************************************************
       UPGRADE-FILE-PD SECTION.
           PERFORM CLEAR-PD-FILE.

      * PD-REC
           MOVE OPD-ID                    TO PD-ID.
           MOVE OPD-BRANCH                TO PD-BRANCH.
           MOVE OPD-SOC-SEC               TO PD-SOC-SEC.

      * PD-RECORD (MATCHES LOAN FILE)
           MOVE OPD-OWNBR                 TO PD-OWNBR.
           MOVE OPD-NUMBER                TO PD-ACCTNO.
           MOVE OPD-CLASS                 TO PD-CLASS.
           MOVE OPD-SUBCLASS              TO PD-SUBCLASS.
           MOVE OPD-LAWCODE               TO PD-LAWCODE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE OPD-SSNO   (SUB)       TO PD-SSNO   (SUB)
              MOVE OPD-MAKERCD(SUB)       TO PD-MAKERCD(SUB)
           END-PERFORM.

           MOVE OPD-ORGST                 TO PD-ORGST.
           MOVE OPD-ORGBR                 TO PD-ORGBR.
           MOVE OPD-OWNST                 TO PD-OWNST.
           MOVE OPD-SPRCLASS              TO PD-SPRCLASS.
           MOVE OPD-CONVERCD              TO PD-CONVERCD.
           MOVE OPD-MIDSTREAM             TO PD-MIDSTREAM.

           MOVE OPD-PURCD                 TO PD-PURCD.
           MOVE OPD-SRCD                  TO PD-SRCD.
           MOVE OPD-COLLCD                TO PD-COLLCD.
           MOVE OPD-COLLID                TO PD-COLLID.
           MOVE OPD-POCD                  TO PD-POCD.
           MOVE OPD-PLCD                  TO PD-PLCD.
           MOVE OPD-PLTYPE                TO PD-PLTYPE.
           MOVE OPD-CONFIDENTCD           TO PD-CONFIDENTCD.
           MOVE OPD-ALLOTCD               TO PD-ALLOTCD.
           MOVE OPD-RECASTCD              TO PD-RECASTCD.
           MOVE OPD-SKIP-TRW              TO PD-SKIP-TRW.
           MOVE OPD-STATUSFG              TO PD-STATUSFG.
           MOVE OPD-SMALLOAN              TO PD-SMALLOAN.
           MOVE OPD-SALEFIN               TO PD-SALEFIN.
           MOVE OPD-SERVACCT              TO PD-SERVACCT.
           MOVE OPD-LOANTYPE              TO PD-LOANTYPE.
           MOVE OPD-ADDON-FG              TO PD-ADDON-FG.
           MOVE OPD-PAY-SCHLD-FG          TO PD-PAY-SCHLD-FG.
           MOVE OPD-LC-CODE               TO PD-LC-CODE.
           MOVE OPD-CAN-LOAN-NO           TO PD-CAN-LOAN-NO.
           MOVE OPD-STATFLAGS             TO PD-STATFLAGS.

           MOVE OPD-ENTDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-ENTDATE.

           MOVE OPD-LOANDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-LOANDATE.

           MOVE OPD-PURDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-PURDATE.

           MOVE OPD-INTDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-INTDATE.

           MOVE OPD-CASHDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-CASHDATE.


           MOVE 0                         TO PD-FILEDATE
                                             PD-RESCISSION-DATE
                                             PD-PART-RECOURSE-DATE.

      * REDEFINES FILEDATE, RESCISSION-DATE, PART-RECOURSE-DATE

           MOVE OPD-FILEDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

           IF OPD-RESCISSION = "Y"
              MOVE DATE-YYYYMMDD       TO PD-RESCISSION-DATE
           ELSE
              MOVE DATE-YYYYMMDD       TO PD-FILEDATE.

           MOVE OPD-ORIG-1STPAY              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-ORIG-1STPYDATE.

           MOVE OPD-1STPYDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-1STPYDATE.

           MOVE OPD-JDDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-JDDATE.

           MOVE OPD-PLDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-PLDATE.

           MOVE OPD-ACCELDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-ACCELDATE.

           MOVE OPD-REDUDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-REDUDATE.

           MOVE OPD-POFFDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-POFFDATE.

           MOVE OPD-LEGALDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-LEGALDATE.

           MOVE OPD-BNKRPTDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-BNKRPTDATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    A15: OPD-BNKRPT-DISMISSAL IS A REDEFINE OF OPD-CYCLE-DATE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                    TO PD-BNKRPT-DISMISS-DATE.

           IF OPD-BNKRPT-DISMISSAL NUMERIC
              IF OPD-BNKRPT-DISMISSAL NOT = 202020 AND NOT = 020202
                                  AND NOT = SPACES
                 MOVE OPD-BNKRPT-DISMISSAL TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO PD-BNKRPT-DISMISS-DATE.
                
           MOVE OPD-DEBTAIDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-DEBTAIDATE.

           MOVE OPD-REPODATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-REPODATE.

           MOVE OPD-REDEEMDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-REDEEMDATE.

           MOVE OPD-SOLDDATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-SOLDDATE.

           MOVE OPD-INS-CONFIRMATION              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-INS-CONFIRM-DATE.

           MOVE OPD-PRIOR-RN-FOR10              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-PRIOR-RN-FOR10-DATE.

           MOVE OPD-LAST-RN-FOR10              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-LAST-RN-FOR10-DATE.

           MOVE OPD-TRW-DOFD              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-TRW-DOFD-DATE.

           MOVE OPD-LAST-PYPA-DATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-LAST-PYPA-DATE.

           MOVE OPD-FOLLOWUP-DATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-FOLLOWUP-DATE.

      * START THE METRO2 FIELD WITH DATE-PAID-LAST
           MOVE OPD-DATE-PAID-LAST          TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-METRO2-LAST-PMT-DATE.

           MOVE OPD-LAST-ACCRUAL-DATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-LAST-ACCRUAL-DATE.


           MOVE OPD-APRATE                TO PD-APRATE.
           MOVE OPD-EFFRATE               TO PD-EFFRATE.
           MOVE OPD-SMPRATE               TO PD-SMPRATE.
           MOVE OPD-YLDRATE               TO PD-YLDRATE.
           MOVE OPD-OVRDRATE              TO PD-OVRDRATE.
           MOVE OPD-ENDORATE              TO PD-ENDORATE.
           MOVE OPD-JDRATE                TO PD-JDRATE.
           MOVE OPD-ACCRATE               TO PD-ACCRATE.
           MOVE OPD-REDURATE              TO PD-REDURATE.
           MOVE OPD-CONRATE               TO PD-CONRATE.
           MOVE OPD-SERVPER               TO PD-SERVPER.

           MOVE OPD-SELLPR                TO PD-SELLPR.
           MOVE OPD-DOWNPY                TO PD-DOWNPY.
           MOVE OPD-TRADEIN               TO PD-TRADEIN.
           MOVE OPD-LNAMT                 TO PD-LNAMT.
           MOVE OPD-FINCHG                TO PD-FINCHG.
           MOVE OPD-INTCHG                TO PD-INTCHG.
           MOVE OPD-SERCHG                TO PD-SERCHG.
           MOVE OPD-EXTCHG                TO PD-EXTCHG.
           MOVE OPD-MAINTFEE              TO PD-MAINTFEE.
           MOVE OPD-POINTS                TO PD-POINTS.
           MOVE OPD-PROCEEDS              TO PD-PROCEEDS.

           MOVE OPD-ORGTERM               TO PD-ORGTERM.
           MOVE OPD-1STPYAMT              TO PD-1STPYAMT.
           MOVE OPD-NOREGPY               TO PD-NOREGPY.
           MOVE OPD-REGPYAMT              TO PD-REGPYAMT.
           MOVE OPD-LASTPYAMT             TO PD-LASTPYAMT.
           MOVE OPD-PROCASH               TO PD-PROCASH.

           IF OPD-TILLNO = "1"
              MOVE 1  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "2"
              MOVE 2  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "3"
              MOVE 3  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "4"
              MOVE 4  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "5"
              MOVE 5  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "6"
              MOVE 6  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "7"
              MOVE 7  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "8"
              MOVE 8  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "9"
              MOVE 9  TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "A"
              MOVE 10 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "B"
              MOVE 11 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "C"
              MOVE 12 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "D"
              MOVE 13 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "E"
              MOVE 14 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "F"
              MOVE 15 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "G"
              MOVE 16 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "H"
              MOVE 17 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "I"
              MOVE 18 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "J"
              MOVE 19 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "K"
              MOVE 20 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "L"
              MOVE 21 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "M"
              MOVE 22 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "N"
              MOVE 23 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "O"
              MOVE 24 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "P"
              MOVE 25 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "Q"
              MOVE 26 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "R"
              MOVE 27 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "S"
              MOVE 28 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "T"
              MOVE 29 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "U"
              MOVE 30 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "V"
              MOVE 31 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "W"
              MOVE 32 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "X"
              MOVE 33 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "Y"
              MOVE 34 TO PD-TILLNO
           ELSE
           IF OPD-TILLNO = "Z"
              MOVE 35 TO PD-TILLNO.

           MOVE OPD-STATE-FLAG            TO PD-STATE-FLAG.
           MOVE OPD-STATE-WORKER          TO PD-STATE-WORKER.
           MOVE OPD-STATE-WORKER2         TO PD-STATE-WORKER2.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE OPD-FEECODE  (SUB)     TO PD-FEECODE  (SUB)
              MOVE OPD-FEEAMT   (SUB)     TO PD-FEEAMT   (SUB)
              MOVE OPD-FEEOURS  (SUB)     TO PD-FEEOURS  (SUB)
              MOVE OPD-FEEFINCHG(SUB)     TO PD-FEEFINCHG(SUB)
           END-PERFORM.

           MOVE OPD-INSURED               TO PD-INSURED.
           MOVE OPD-INSTYPES              TO PD-INSTYPES.
           MOVE OPD-INSELIGIBLE           TO PD-INSELIGIBLE.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              MOVE OPD-INSCOMP      (SUB) TO PD-INSCOMP      (SUB)
              MOVE OPD-INSOURS      (SUB) TO PD-INSOURS      (SUB)
              MOVE OPD-INSEFF       (SUB) TO PD-INSEFF-DATE  (SUB)
              IF OPD-INSEFF(SUB) > 900
                 MOVE OPD-INSEFF(SUB) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 MOVE DATE-YYYYMMDD TO PD-INSEFF-DATE(SUB)
              END-IF
              MOVE OPD-INSEXP       (SUB) TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD          TO PD-INSEXP-DATE  (SUB)
              MOVE OPD-INSPREM      (SUB) TO PD-INSPREM      (SUB)
              MOVE OPD-INSCOVR      (SUB) TO PD-INSCOVR      (SUB)
              MOVE OPD-INSCOMM      (SUB) TO PD-INSCOMM      (SUB)
              MOVE OPD-INSANTICERN  (SUB) TO PD-INSANTICERN  (SUB)
              MOVE OPD-INSANTICADJ  (SUB) TO PD-INSANTICADJ  (SUB)
              MOVE OPD-INSUFERN     (SUB) TO PD-INSUFERN     (SUB)
              MOVE OPD-INSACCERN    (SUB) TO PD-INSACCERN    (SUB)
              MOVE OPD-INSEARN-ORDER(SUB) TO PD-INSEARN-ORDER(SUB)
              MOVE OPD-INSFINCHG    (SUB) TO PD-INSFINCHG    (SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * THIS WILL MOVE THE MMYY FROM THE OLD TO THE NEW UNTIL WE CHANGE
      * THE WAY IT WORKS IN A30.
           MOVE OPD-PYMMYY-RECDEL         TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM               TO PD-PY-YYYYMM-RECDEL.
           MOVE OPD-MTD-PAYS-RECDEL       TO PD-MTD-PAYS-RECDEL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE: OPD-GARNISH-EXTR IS A REDEFINE OF OPD-GIFTNET-EXTR
      *          IN A15.  PD-GIFTNET-EXTR WAS REMOVED IN A30.
      *          PD-GARNISH-EXTR IS USED FOR MODERN FINANCE.
      *          2017-0609, BETH & JAY DISCUSSED AND MOVING 
      *          OPD-GARNISH-EXTR TO PD-GARNISH-EXTR JUST IN CASE
      *          EVENTHOUGH MAY NEVER BE USED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE OPD-GIFTNET-EXTR          TO PD-GIFTNET-EXTR.
           MOVE OPD-GARNISH-EXTR          TO PD-GARNISH-EXTR.

           MOVE OPD-SHIP-SAME-FG          TO PD-SHIP-SAME-FG.
           MOVE OPD-AD-D-INS-COVR         TO PD-AD-D-INS-COVR.
           MOVE OPD-TOT-PARTIAL-CHGOFF    TO PD-TOT-PARTIAL-CHGOFF.
           MOVE OPD-LPO                   TO PD-LPO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE: OPD-EMP-ID IS A REDEFINES OF OPD-REP-CD IN A15.
      *          PD-EMP-ID WAS SEPERATED FROM PD-REP-CD IN A30.
      *          PD-EMP-ID IS POPULATED FROM OPD-EMP-ID IF WORLD.
      *          PD-REP-CD IS POPULATED FROM OPD-REP-CD NOT WORLD.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( OPD-EMP-ID NUMERIC )
              MOVE OPD-EMP-ID             TO PD-EMP-ID
           ELSE
              MOVE OPD-REP-CD             TO PD-REP-CD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE OPD-PROCESS-CD            TO PD-PROCESS-CD.
           MOVE OPD-WCBC-CREDITLIM        TO PD-WCBC-CREDITLIM.
           MOVE OPD-FOLLOWUP-CD           TO PD-FOLLOWUP-CD.
           MOVE OPD-MIP-FG                TO PD-MIP-FG.
           MOVE OPD-MIP-MTD-CLAHPAID      TO PD-MIP-MTD-CLAHPAID.

           MOVE OPD-MIP-CLPDTH            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-MIP-CLPDTH-DATE.

           MOVE OPD-MIP-TOT-CLPAID        TO PD-MIP-TOT-CLPAID.
           IF OPD-MIP-TOT-CLPAID = 0202020.20 OR 2020202.02
              MOVE 0 TO PD-MIP-TOT-CLPAID PD-IL-GROSS-INCOME.

           MOVE OPD-MIP-AHPDTH            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-MIP-AHPDTH-DATE.

           MOVE OPD-MIP-TOT-AHPAID        TO PD-MIP-TOT-AHPAID.
           IF OPD-MIP-TOT-AHPAID = 0202020.20 OR 2020202.02
              MOVE 0 TO PD-MIP-TOT-AHPAID PD-IL-GROSS-INCOME2.

           MOVE OPD-RESCISSION            TO PD-RESCISSION.
           MOVE OPD-INTERIM-TRW           TO PD-INTERIM-TRW.
           MOVE OPD-DLNO                  TO PD-DLNO.
           MOVE OPD-DLPOOL                TO PD-DLPOOL.
           MOVE OPD-DLRECOURSE            TO PD-DLRECOURSE.
           MOVE OPD-DLPARTRCAMT           TO PD-DLPARTRCAMT.
           MOVE OPD-ORIGPURAMT            TO PD-ORIGPURAMT.
           MOVE OPD-PURBAL                TO PD-PURBAL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE OPD-DLRVPRIN(SUB)     TO PD-DLRVPRIN(SUB)
               MOVE OPD-DLRVCHGS(SUB)     TO PD-DLRVCHGS(SUB)
           END-PERFORM.

           MOVE OPD-DLPARVHELD            TO PD-DLPARVHELD.
           MOVE OPD-DLPARVPAID            TO PD-DLPARVPAID.
           MOVE OPD-DLDISC                TO PD-DLDISC.

           MOVE OPD-LCPDTH            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-LCPDTH-DATE.

           MOVE OPD-INTPDTH            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-INTPDTH-DATE.

           MOVE OPD-CURBAL                TO PD-CURBAL.
           MOVE OPD-INTBAL                TO PD-INTBAL.
           MOVE OPD-LCBAL                 TO PD-LCBAL.
           MOVE OPD-OTHBAL                TO PD-OTHBAL.
           MOVE OPD-OT2BAL                TO PD-OT2BAL.
           MOVE OPD-LCPAID                TO PD-LCPAID.
           MOVE OPD-LCPARTIALS            TO PD-LCPARTIALS.
           MOVE OPD-STMT-COUNTER          TO PD-STMT-COUNTER.
           MOVE OPD-PRIOR-STMT-COUNTER    TO PD-PRIOR-STMT-COUNTER.

           MOVE SPACES                    TO PD-FROZEN-REASON.
      *    MOVE OPD-COUPON-STATEMENT      TO PD-COUPON-STATEMENT.
           MOVE OPD-MONTHEND-BAL          TO PD-MONTHEND-BAL.

           MOVE OPD-ANTICTERM             TO PD-ANTICTERM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE OPD-ANTICERN  (SUB)    TO PD-ANTICERN  (SUB)
              MOVE OPD-ANTICADJ  (SUB)    TO PD-ANTICADJ  (SUB)
              MOVE OPD-UFERN     (SUB)    TO PD-UFERN     (SUB)
              MOVE OPD-ACCERN    (SUB)    TO PD-ACCERN    (SUB)
              MOVE OPD-EARN-ORDER(SUB)    TO PD-EARN-ORDER(SUB)
           END-PERFORM.

           MOVE OPD-LMO-PRIOR-UNEARNED    TO PD-LMO-PRIOR-INTEREST.
           MOVE OPD-PRIOR-UNEARNED        TO PD-PRIOR-UNEARNED.
           MOVE OPD-ERNDELQ-STOP          TO PD-ERNDELQ-STOP.
           MOVE OPD-NONACRERN             TO PD-NONACRERN.
           MOVE OPD-PRIOR-NONACRERN       TO PD-PRIOR-NONACRERN.

           MOVE OPD-ERN-IB-PDTH            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-ERN-IB-PDTH-DATE.

           MOVE OPD-ERN-IB-PRINBAL        TO PD-ERN-IB-PRINBAL.
           MOVE OPD-ERN-IB-INTOWE         TO PD-ERN-IB-INTOWE.
           MOVE OPD-TOTERN                TO PD-TOTERN.
           MOVE OPD-TOTOTH                TO PD-TOTOTH.
           MOVE OPD-TOTINT                TO PD-TOTINT.
           MOVE OPD-TOTLCHG               TO PD-TOTLCHG.
           MOVE OPD-TOTDEF                TO PD-TOTDEF.
           MOVE OPD-TOTSERACCT            TO PD-TOTSERACCT.
           MOVE OPD-LCWAIVERS             TO PD-LCWAIVERS.
           MOVE OPD-INTWAIVERS            TO PD-INTWAIVERS.
           MOVE OPD-TOTPAYMNTD            TO PD-TOTPAYMNTD.
           MOVE OPD-TOTEXCPAYMNTD         TO PD-TOTEXCPAYMNTD.
           MOVE OPD-TOTNODEF              TO PD-TOTNODEF.
           MOVE OPD-YTDNODEF              TO PD-YTDNODEF.
           MOVE OPD-TOTNODEFACT           TO PD-TOTNODEFACT.
           MOVE OPD-YTDNODEFACT           TO PD-YTDNODEFACT.
           MOVE OPD-TOTNOALDEL            TO PD-TOTNOALDEL.
           MOVE OPD-YTDNOALDEL            TO PD-YTDNOALDEL.
           MOVE OPD-YTDINTPD              TO PD-YTDINTPD.
           MOVE OPD-LYRINTPD              TO PD-LYRINTPD.
           MOVE OPD-TOTINTPD              TO PD-TOTINTPD.
           MOVE OPD-RECASTINT             TO PD-RECASTINT.
           MOVE OPD-RECASTPRIN            TO PD-RECASTPRIN.

           MOVE OPD-CL-REBATE             TO PD-CL-REBATE.
           MOVE OPD-AH-REBATE             TO PD-AH-REBATE.
           MOVE OPD-PP-REBATE             TO PD-PP-REBATE.
           MOVE OPD-INT-REBATE            TO PD-INT-REBATE.
           MOVE OPD-SC-REBATE             TO PD-SC-REBATE.
           MOVE OPD-MF-REBATE             TO PD-MF-REBATE.
           MOVE OPD-DF-REBATE             TO PD-DF-REBATE.
           MOVE OPD-EX-REBATE             TO PD-EX-REBATE.
           MOVE OPD-O1-REBATE             TO PD-O1-REBATE.
           MOVE OPD-O2-REBATE             TO PD-O2-REBATE.
           MOVE OPD-O3-REBATE             TO PD-O3-REBATE.
           MOVE OPD-O4-REBATE             TO PD-O4-REBATE.
           MOVE OPD-O5-REBATE             TO PD-O5-REBATE.

           MOVE OPD-PLREASON              TO PD-PLREASON.
           MOVE OPD-COLLECTOR             TO PD-COLLECTOR.
           MOVE OPD-ACTIONCD              TO PD-ACTIONCD.
           MOVE OPD-ACTIONCD2             TO PD-ACTIONCD2.
           MOVE OPD-ATCODE                TO PD-ATCODE.
           MOVE OPD-RECODE                TO PD-RECODE.

           MOVE OPD-PROMDAYS-PASTDUE      TO PD-PROMDAYS-PASTDUE.
           MOVE OPD-PROMID                TO PD-PROMID.

           MOVE OPD-PROMDATE            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-PROMDATE.

           MOVE OPD-PROMAMT               TO PD-PROMAMT.

           MOVE OPD-PROMDATE2             TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-PROMDATE2.

           MOVE OPD-DATE-PAID-LAST            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-DATE-PAID-LAST.

           MOVE OPD-AMT-PAID-LAST         TO PD-AMT-PAID-LAST.
           MOVE OPD-LASTCFORMNO           TO PD-LASTCFORMNO.
           MOVE OPD-LASTCFORMDATE            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-LASTCFORMDATE.
           MOVE OPD-DELCNT                TO PD-DELCNT.
           MOVE OPD-DELCNT2               TO PD-DELCNT2.
           MOVE OPD-CREDITLIM             TO PD-CREDITLIM.
           MOVE OPD-CRSCORE               TO PD-CRSCORE.
           MOVE OPD-LASTSFORMNO           TO PD-LASTSFORMNO.
           MOVE OPD-LASTSFORMDATE            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-LASTSFORMDATE.
           MOVE OPD-SOLCNT                TO PD-SOLCNT.
           MOVE OPD-MESSAGE               TO PD-MESSAGE.

           MOVE OPD-OLDACCTNO             TO OLD-ACCTNO.
           MOVE NEW-ACCTNO                TO PD-OLDACCTNO.

           MOVE OPD-PRLNNO                TO OLD-ACCTNO.
           MOVE NEW-ACCTNO                TO PD-PRLNNO.

           MOVE OPD-PRBAL                 TO PD-PRBAL.
           MOVE OPD-PRINTDUE              TO PD-PRINTDUE.
           MOVE OPD-PRFINCHGRB            TO PD-PRFINCHGRB.
           MOVE OPD-PRINSRB               TO PD-PRINSRB.
           MOVE OPD-PRPOCD                TO PD-PRPOCD.
           MOVE OPD-PRPODATE            TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD             TO PD-PRPODATE.
           MOVE OPD-PRDELINQAMT           TO PD-PRDELINQAMT.
           MOVE OPD-PRLNAMT               TO PD-PRLNAMT.
           MOVE OPD-PRPDUE-DAYS           TO PD-PRPDUE-DAYS.

           MOVE OPD-PRDLDISC-UNEARN       TO PD-PRDLDISC-UNEARN.
           MOVE OPD-PRCONPROFILE          TO PD-PRCONPROFILE.
           MOVE OPD-SUM-ORIGPURAMT        TO PD-SUM-ORIGPURAMT.
           MOVE OPD-SUM-LNAMT             TO PD-SUM-LNAMT.
           MOVE OPD-SUM-FINCHG            TO PD-SUM-FINCHG.
           MOVE OPD-SUM-INTCHG            TO PD-SUM-INTCHG.
           MOVE OPD-SUM-ORGTERM           TO PD-SUM-ORGTERM.
           MOVE OPD-SUM-REGPYAMT          TO PD-SUM-REGPYAMT.
           MOVE OPD-SUM-EXTCHG            TO PD-SUM-EXTCHG.
           MOVE OPD-SUM-SERCHG            TO PD-SUM-SERCHG.
           MOVE OPD-SUM-MAINTFEE          TO PD-SUM-MAINTFEE.

           MOVE OPD-UNITPER-CD            TO PD-UNITPER-CD.
           MOVE OPD-UNITPER-FREQ          TO PD-UNITPER-FREQ.
           MOVE OPD-DAYS-IN-UNITPER       TO PD-DAYS-IN-UNITPER.
           MOVE OPD-UNITPER-PER-YEAR      TO PD-UNITPER-PER-YEAR.
           MOVE OPD-REPOCD                TO PD-REPOCD.
           MOVE OPD-BR-TFR-FG             TO PD-BR-TFR-FG.
           MOVE OPD-O3INS-DLPROC          TO PD-O3INS-DLPROC.
           MOVE OPD-INSPREM-DLADJ         TO PD-INSPREM-DLADJ.
           MOVE OPD-PLAMT                 TO PD-PLAMT.
           MOVE OPD-MTD-PRIN              TO PD-MTD-PRIN.
           MOVE OPD-MTD-LC                TO PD-MTD-LC.
           MOVE OPD-MTD-INT               TO PD-MTD-INT.
           MOVE OPD-MTD-OT                TO PD-MTD-OT.
           MOVE OPD-MTD-EX                TO PD-MTD-EX.
           MOVE OPD-BNKRPT-CHAPTER        TO PD-BNKRPT-CHAPTER.
           MOVE OPD-BMO-CON-STATUS        TO PD-BMO-CON-STATUS.
           MOVE OPD-BMO-REC-STATUS        TO PD-BMO-REC-STATUS.

           IF ( OPD-APPNO = ZEROES )
              MOVE ZEROES                 TO PD-APP-NUMBER
           ELSE
              MOVE OPD-APPNO              TO WSS-MA-APPNO
              MOVE 01                     TO WSS-MA-SEQNO
              MOVE WSS-MA-APP-NUMBER      TO PD-APP-NUMBER.

           MOVE OPD-GIFTNET               TO PD-GIFTNET.
           MOVE OPD-GIFTNET-OUR-PO        TO PD-GIFTNET-OUR-PO.
           MOVE OPD-PROC-DISBURSED        TO PD-PROC-DISBURSED.
           MOVE OPD-NOODDPY               TO PD-NOODDPY.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE OPD-CONPROFILE(SUB)    TO PD-CONPROFILE(SUB)
              MOVE OPD-RECPROFILE(SUB)    TO PD-RECPROFILE(SUB)
           END-PERFORM
           MOVE OPD-TRW-STATUS            TO PD-TRW-STATUS.
           MOVE OPD-ORIGIN                TO PD-ORIGIN.
           MOVE OPD-PO-TRCD               TO PD-PO-TRCD.

           MOVE OPD-NEXIS-CRSCORE      TO PD-NEXIS-CRSCORE.
           MOVE OPD-TRW-B-CONSUMER-IND    TO PD-TRW-B-CONSUMER-IND.

      *?   MOVE OPD-PARVHELD-PAID-FG      TO PD-PARVHELD-PAID-FG.

           MOVE OPD-ACCT-FROZEN           TO PD-ACCT-FROZEN.
           MOVE OPD-TRW-SPECIAL-CODE      TO PD-TRW-SPECIAL-CODE.

           MOVE OPD-CREATION-STATUS       TO PD-CREATION-STATUS.
           MOVE OPD-TOTNOLCHG             TO PD-TOTNOLCHG.
           MOVE OPD-LBOX                  TO PD-LBOX.
           MOVE OPD-EXTRA-PRIN-LFP        TO PD-EXTRA-PRIN-LFP.
           MOVE OPD-REMAIN-INT-SINCE-LFP  TO PD-REMAIN-INT-SINCE-LFP.
           MOVE OPD-REMAIN-PRIN-SINCE-LFP TO PD-REMAIN-PRIN-SINCE-LFP.

           MOVE OPD-INTERIM-COUPON        TO PD-INTERIM-COUPON.
           MOVE OPD-DLCOMM-O3-CMSI        TO PD-DLCOMM-O3-CMSI.
           MOVE OPD-DLCOMM-PP-CMSI        TO PD-DLCOMM-PP-CMSI.
           MOVE OPD-TRW-J-CONSUMER-IND    TO PD-TRW-J-CONSUMER-IND.
           MOVE OPD-ACH                   TO PD-ACH.

      *?   MOVE OPD-1099-C-FG             TO PD-1099-C-FG.
      *?   MOVE OPD-TRW-COMPLIANCE-CODE   TO PD-TRW-COMPLIANCE-CODE.
      *?   MOVE OPD-UNWIND-REASON-CODE    TO PD-UNWIND-REASON-CODE.

      *- - - - - - - - - - - - - - - - - - - - - -{ NEW FIELDS SKIPPED }
      *        05  PD-PBI-PBD-FG               PIC X.
      *        05  PD-OLD-ACCT-FORMAT-FG       PIC X.
      *        05  PD-PAPERLESS-STMT-FG        PIC X.
      *        05  PD-APPROVED-BY-USER         PIC X(10).
      *        05  PD-APPROVED-BY-CODE         PIC X(3).
      *        05  PD-APPROVED-BY-DATE         PIC 9(8)     COMP-3.
      *        05  PD-APPROVED-BY-TIME         PIC 9(6)     COMP-3.
      *        05  PD-TAX-AMT                  PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT1              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT2              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT3              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT4              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT5              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT6              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT7              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT8              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT9              PIC S9(7)V99 COMP-3.
      *        05  PD-FUTURE-AMT10             PIC S9(7)V99 COMP-3.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES                     TO PD-LTOUCH-DATE.
           MOVE ZEROES                     TO PD-LTOUCH-TIME.

      *  ^^^^^^^END OF PD-RECORD FIELDS^^^^^^
      * OPD-REST-OF-REC.

           MOVE OPD-MSG1                  TO PD-MSG1.
           MOVE OPD-MSG2                  TO PD-MSG2.
           MOVE OPD-DLPAID                TO PD-DLPAID.
           MOVE OPD-CKNO                  TO PD-CKNO.

      * OPD-FREE-SPACE.
      *   MOVE REDEFINES FIELDS INDIVIDUALLY AS SOME OF THE SIZES HAVE CHANGED
      *    MOVE OPD-FREE1                 TO PD-FREE1.
      *    MOVE OPD-FREE2                 TO PD-FREE2.

      * VVVVVV  OPD-NEW-FIELDS REDEFINES PD-FREE-SPACE.
           MOVE OPD-FINAMT                TO PD-FINAMT.
           MOVE OPD-PROCEEDS-FG           TO PD-PROCEEDS-FG.
           MOVE OPD-POINTS-FG             TO PD-POINTS-FG.
           MOVE OPD-OVRDRATE-FG           TO PD-OVRDRATE-FG.
           MOVE OPD-SERCHG-FG             TO PD-SERCHG-FG.
           MOVE OPD-MAINTFEE-FG           TO PD-MAINTFEE-FG.
           MOVE OPD-EXTCHG-FG             TO PD-EXTCHG-FG.
           MOVE OPD-REGPYAMT-FG           TO PD-REGPYAMT-FG.

           MOVE OPD-PRPAID-AHEAD          TO PD-PRPAID-AHEAD.
           MOVE OPD-RESTRICT-ADJ          TO PD-RESTRICT-ADJ.
           MOVE OPD-DUAL-LOAN-ADJ         TO PD-DUAL-LOAN-ADJ.
           MOVE OPD-CAN-LOAN-MAX          TO PD-CAN-LOAN-MAX.
           MOVE OPD-PERDOWN               TO PD-PERDOWN.

           MOVE OPD-SELLPR-FG             TO PD-SELLPR-FG.
           MOVE OPD-DOWNPY-FG             TO PD-DOWNPY-FG.
           MOVE OPD-PERDOWN-FG            TO PD-PERDOWN-FG.
           MOVE OPD-SELL-DOWN-FG          TO PD-SELL-DOWN-FG.
           MOVE OPD-BALLOON-FG            TO PD-BALLOON-FG.
           MOVE OPD-BALLOON-LAST-FG       TO PD-BALLOON-LAST-FG.

           MOVE OPD-DLDISC-FG             TO PD-DLDISC-FG.
           MOVE OPD-DLPARVHELD-FG         TO PD-DLPARVHELD-FG.
           MOVE OPD-DLPARVPAID-FG         TO PD-DLPARVPAID-FG.
           MOVE OPD-ONE-MTH-NO-DAY-EXT-SW TO PD-ONE-MTH-NO-DAY-EXT-SW.
           MOVE OPD-ASSIGN-RN-ACCTNO      TO PD-ASSIGN-RN-ACCTNO.
           MOVE OPD-APR-FINAMT            TO PD-APR-FINAMT.
           MOVE OPD-DLCOMM-PP-CMSI-FG     TO PD-DLCOMM-PP-CMSI-FG.
           MOVE OPD-DLCOMM-O3-CMSI-FG     TO PD-DLCOMM-O3-CMSI-FG.

      * ^^^^^^  END OPD-NEW-FIELDS REDEFINES PD-FREE-SPACE.

      * VVVVVV ADITIONAL FIELDS MAPPED TO WK-REC
           MOVE OPD-POSTING-BR            TO PD-POSTING-BR.
           MOVE OPD-OTHBR-FG              TO PD-OTHBR-FG.
           MOVE OPD-APPROVAL-LIMIT        TO PD-APPROVAL-LIMIT.
           MOVE OPD-APPROVAL-USERID       TO PD-APPROVAL-USERID.
           MOVE OPD-MIN-CRED-SCORE        TO PD-MIN-CRED-SCORE.
           MOVE OPD-CRED-SCORE-USERID     TO PD-CRED-SCORE-USERID.
      * ^^^^^^ ADITIONAL FIELDS MAPPED TO WK-REC

      * PD2 KEY
           MOVE OPD2-ID                   TO PD2-ID.
           MOVE OPD2-BRANCH               TO PD2-BRANCH.
           MOVE OPD2-BWNAME               TO PD2-BWNAME.

      *  FIELDS FROM  NEW FIELDS REDEFINITION THAT HAVE OCCURS
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              IF SUB <= 10
                 MOVE OPD-FEECD(SUB)        TO PD-FEECD(SUB)
                 MOVE OPD-INS-FG(SUB)       TO PD-INS-FG(SUB)
                 MOVE OPD-INS-TERM-SW(SUB)  TO PD-INS-TERM-SW(SUB)
                 MOVE OPD-COVERAGE-SW(SUB)  TO PD-COVERAGE-SW(SUB)
                 MOVE OPD-PREMIUM-SW(SUB)   TO PD-PREMIUM-SW(SUB)
              END-IF
              IF SUB <= 3
                 MOVE OPD-DLRVPRIN-FG(SUB) TO PD-DLRVPRIN-FG(SUB)
                 MOVE OPD-DLRVCHGS-FG(SUB) TO PD-DLRVCHGS-FG(SUB)
              END-IF
              IF SUB <= 2
                 MOVE OPD-FEECD-FG(SUB) TO PD-FEECD-FG(SUB)
              END-IF
           END-PERFORM.

       UPGRADE-FILE-PD-EXIT.
           EXIT.
