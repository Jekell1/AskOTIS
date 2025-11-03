      * COPYMEMBER: LIBUP/UPATRLCL

      ***********************************************************************
      *      ASCII DIRECT LOAN TRANSACTION FILE CLEAR ROUTINE
      *
      ***********************************************************************
       CLEAR-ATRL-FILE SECTION.
           MOVE ALL X"09" TO ATRL-REC.

           MOVE 0 TO
              ATL-DATE ATL-BRNO ATL-CLASS ATL-ACCTNO 
              ATL-LAWCODE ATL-LINE ATL-SUBCLASS ATL-LOANDATE ATL-INTDATE
              ATL-PROCEEDS ATL-LNAMT ATL-FINCHG ATL-INTCHG ATL-SERCHG
              ATL-EXTCHG ATL-MAINTFEE ATL-ORGTERM ATL-1STPYAMT
              ATL-NOREGPY ATL-REGPYAMT ATL-LASTPYAMT
              ATL-PROCASH ATL-TILLNO ATL-DLNO
              ATL-PRLNNO ATL-PRBAL ATL-PRINTDUE ATL-PRFINCHGRB
              ATL-PRINSRB ATL-STATE-WORKER ATL-RESTRICTD-ACCTNO
              ATL-SPRCLASS ATL-POSTDATE ATL-POSTTIME ATL-APR
              ATL-PRDLDISC-UNEARN ATL-POINTS ATL-TAX-AMT
              ATL-APPROVED-BY-DATE ATL-APPROVED-BY-TIME ATL-LTOUCH-DATE
              ATL-DEBIT-CARD-AMT.

           MOVE SPACES TO ATL-PURCD ATL-SRCD ATL-COLLCD ATL-SERVACCT
                          ATL-LOANTYPE ATL-INSURED ATL-INSTYPES
                          ATL-INSELIGIBLE ATL-LPEOD5-FG ATL-USERID
                          ATL-GIFTNET ATL-SP-LNCOST-FRMLA
                          ATL-MIP-FG ATL-INS-TAX-COUNTY-CD
                          ATL-INS-TAX-CITY-CD ATL-PBI-PBD-FG
                          ATL-APPROVED-BY-USER ATL-APPROVED-BY-CODE.

              PERFORM CLEAR-ATRL-FILE-TABLES
                   VARYING TRLSUB FROM 1 BY 1 UNTIL TRLSUB > 20.

            GO TO CLEAR-ATRL-FILE-EXIT.

      ********************************************
       CLEAR-ATRL-FILE-TABLES.
      ********************************************
           IF TRLSUB < 5
              MOVE 0      TO ATL-SSNO(TRLSUB)
              MOVE SPACES TO ATL-MAKERCD(TRLSUB).
           IF TRLSUB < 11
              MOVE 0 TO ATL-EARNINGS(TRLSUB) ATL-ANTICERN(TRLSUB) 
              MOVE 0 TO ATL-INSCOMP(TRLSUB) ATL-INSEFF-DATE(TRLSUB)
                        ATL-INSEXP-DATE(TRLSUB) ATL-INSPREM(TRLSUB)
                        ATL-INSCOMM(TRLSUB) ATL-INSANTICERN(TRLSUB)
                        ATL-INSEARNINGS(TRLSUB)
              MOVE SPACES TO ATL-INSOURS(TRLSUB).

           MOVE 0      TO ATL-FEEAMT(TRLSUB).
           MOVE SPACES TO ATL-FEECODE(TRLSUB).

       CLEAR-ATRL-FILE-EXIT.
           EXIT.
